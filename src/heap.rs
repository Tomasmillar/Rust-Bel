use crate::error::{BelError, BelResult};
use crate::value::{BelValue, PairId};

/// A single cons cell on the heap.
pub struct ConsCell {
    pub car: BelValue,
    pub cdr: BelValue,
    pub mark: bool,
}

/// The cons cell heap. All pairs are allocated here.
/// PairId is an index into `cells`.
pub struct Heap {
    cells: Vec<ConsCell>,
    free_list: Vec<PairId>,
    capacity: usize,
    /// Number of allocations since last GC (for triggering).
    allocs_since_gc: usize,
    /// GC threshold: trigger GC when allocs_since_gc reaches this.
    gc_threshold: usize,
}

impl Heap {
    pub fn new(capacity: usize) -> Self {
        Heap {
            cells: Vec::with_capacity(1024),
            free_list: Vec::new(),
            capacity,
            allocs_since_gc: 0,
            gc_threshold: 1024 * 64, // Start with 64K allocations between GCs
        }
    }

    /// Allocate a new cons cell. Returns a PairId.
    /// Returns Err(HeapOverflow) if capacity is exceeded.
    pub fn alloc(&mut self, car: BelValue, cdr: BelValue) -> BelResult<PairId> {
        self.allocs_since_gc += 1;

        if let Some(id) = self.free_list.pop() {
            let cell = &mut self.cells[id.0 as usize];
            cell.car = car;
            cell.cdr = cdr;
            cell.mark = false;
            return Ok(id);
        }

        if self.cells.len() >= self.capacity {
            return Err(BelError::HeapOverflow);
        }

        let id = PairId(self.cells.len() as u32);
        self.cells.push(ConsCell {
            car,
            cdr,
            mark: false,
        });
        Ok(id)
    }

    /// Get the car of a pair.
    #[inline]
    pub fn car(&self, id: PairId) -> BelValue {
        self.cells[id.0 as usize].car
    }

    /// Get the cdr of a pair.
    #[inline]
    pub fn cdr(&self, id: PairId) -> BelValue {
        self.cells[id.0 as usize].cdr
    }

    /// Set the car of a pair (xar primitive).
    #[inline]
    pub fn set_car(&mut self, id: PairId, val: BelValue) {
        self.cells[id.0 as usize].car = val;
    }

    /// Set the cdr of a pair (xdr primitive).
    #[inline]
    pub fn set_cdr(&mut self, id: PairId, val: BelValue) {
        self.cells[id.0 as usize].cdr = val;
    }

    /// Get car of a BelValue if it's a pair, or Nil if it's Nil.
    pub fn car_val(&self, val: BelValue) -> BelResult<BelValue> {
        match val {
            BelValue::Nil => Ok(BelValue::Nil),
            BelValue::Pair(id) => Ok(self.car(id)),
            _ => Err(BelError::TypeError("car of non-pair non-nil atom".into())),
        }
    }

    /// Get cdr of a BelValue if it's a pair, or Nil if it's Nil.
    pub fn cdr_val(&self, val: BelValue) -> BelResult<BelValue> {
        match val {
            BelValue::Nil => Ok(BelValue::Nil),
            BelValue::Pair(id) => Ok(self.cdr(id)),
            _ => Err(BelError::TypeError("cdr of non-pair non-nil atom".into())),
        }
    }

    /// Build a proper list from a slice of values.
    pub fn list(&mut self, values: &[BelValue]) -> BelResult<BelValue> {
        let mut result = BelValue::Nil;
        for &val in values.iter().rev() {
            let pair = self.alloc(val, result)?;
            result = BelValue::Pair(pair);
        }
        Ok(result)
    }

    /// Build a proper list from an iterator (consumes it, builds in reverse).
    pub fn list_from_iter(&mut self, iter: impl IntoIterator<Item = BelValue>) -> BelResult<BelValue> {
        let items: Vec<_> = iter.into_iter().collect();
        self.list(&items)
    }

    /// Returns true if this value is a proper list.
    pub fn is_proper_list(&self, val: BelValue) -> bool {
        let mut current = val;
        loop {
            match current {
                BelValue::Nil => return true,
                BelValue::Pair(id) => current = self.cdr(id),
                _ => return false,
            }
        }
    }

    /// Collect a proper list into a Vec. Returns None if not a proper list.
    pub fn list_to_vec(&self, val: BelValue) -> Option<Vec<BelValue>> {
        let mut result = Vec::new();
        let mut current = val;
        loop {
            match current {
                BelValue::Nil => return Some(result),
                BelValue::Pair(id) => {
                    result.push(self.car(id));
                    current = self.cdr(id);
                }
                _ => return None,
            }
        }
    }

    /// Returns the number of allocated cells (including free-listed ones).
    pub fn total_cells(&self) -> usize {
        self.cells.len()
    }

    /// Returns the number of cells on the free list.
    pub fn free_count(&self) -> usize {
        self.free_list.len()
    }

    /// Returns the number of live cells (approximate, accurate after GC).
    pub fn live_count(&self) -> usize {
        self.cells.len() - self.free_list.len()
    }

    /// Returns the number of marked cells (call after mark phase).
    pub fn marked_count(&self) -> usize {
        self.cells.iter().filter(|c| c.mark).count()
    }

    /// Check if a cell is marked.
    pub fn is_marked(&self, id: PairId) -> bool {
        self.cells[id.0 as usize].mark
    }

    /// Returns true if we should trigger a GC cycle.
    pub fn should_gc(&self) -> bool {
        self.allocs_since_gc >= self.gc_threshold
    }

    /// Reset alloc counter after GC.
    pub fn reset_gc_counter(&mut self) {
        self.allocs_since_gc = 0;
    }

    /// Adjust GC threshold based on occupancy.
    pub fn adjust_gc_threshold(&mut self) {
        let live = self.live_count();
        let total = self.total_cells();
        if total > 0 {
            let occupancy = live as f64 / total as f64;
            if occupancy > 0.75 {
                // High occupancy: double the threshold to avoid thrashing
                self.gc_threshold = (self.gc_threshold * 2).min(self.capacity);
            }
        }
    }

    // === GC methods ===

    /// Clear all mark bits (phase 1 of mark-sweep).
    pub fn clear_marks(&mut self) {
        for cell in &mut self.cells {
            cell.mark = false;
        }
    }

    /// Mark a value as reachable. If it's a pair, mark it and add to worklist.
    pub fn mark_value(&mut self, val: BelValue, worklist: &mut Vec<PairId>) {
        if let BelValue::Pair(id) = val {
            if !self.cells[id.0 as usize].mark {
                self.cells[id.0 as usize].mark = true;
                worklist.push(id);
            }
        }
    }

    /// Process the mark worklist: for each marked pair, mark its car and cdr.
    pub fn process_worklist(&mut self, worklist: &mut Vec<PairId>) {
        while let Some(id) = worklist.pop() {
            let car = self.cells[id.0 as usize].car;
            let cdr = self.cells[id.0 as usize].cdr;
            self.mark_value(car, worklist);
            self.mark_value(cdr, worklist);
        }
    }

    /// Sweep: collect unmarked cells to free list (phase 2 of mark-sweep).
    pub fn sweep(&mut self) {
        self.free_list.clear();
        for i in 0..self.cells.len() {
            if !self.cells[i].mark {
                self.free_list.push(PairId(i as u32));
                // Clear the cell — use a poison value to detect use-after-free
                self.cells[i].car = BelValue::Symbol(crate::value::SymbolId(0xDEAD));
                self.cells[i].cdr = BelValue::Symbol(crate::value::SymbolId(0xDEAD));
            }
        }
    }
}
