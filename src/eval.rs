use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

use crate::chars::CharTable;
use crate::error::{BelError, BelResult};
use crate::globals::{self, env_lookup, env_set};
use crate::heap::Heap;
use crate::primitives;
use crate::stream::StreamTable;
use crate::symbol::{sym, SymbolTable};
use crate::value::{BelValue, SymbolId};

/// The Bel evaluation machine.
/// All interpreter state lives here so GC can find roots.
pub struct Machine {
    pub heap: Heap,
    pub symbols: SymbolTable,
    pub chars: CharTable,
    pub streams: StreamTable,

    /// The global environment: alist of (name . value) pairs.
    pub globe: BelValue,
    /// The forms dispatch table: alist of (name . handler-closure).
    pub forms: BelValue,
    /// The prims arity table: ((2-arg-prims) (1-arg-prims) (0-arg-prims))
    pub prims: BelValue,
    /// Unique smark pair — used to tag stack metadata entries.
    pub smark: BelValue,
    /// Unique vmark pair — used to tag user-defined virtual types.
    pub vmark: BelValue,
    /// Virtual function dispatch table: alist of (tag . handler)
    pub virfns: BelValue,
    /// Location function dispatch table: list of (test handler)
    pub locfns: BelValue,

    /// Step counter for safety (fuel).
    pub fuel: u64,
    pub max_fuel: u64,
    /// Ctrl+C interrupt flag.
    pub interrupted: Arc<AtomicBool>,
    /// Debug trace flag.
    pub trace: bool,
    /// Use native arithmetic fast-path (default true).
    pub native_arithmetic: bool,
}

impl Machine {
    pub fn new(heap_capacity: usize, max_fuel: u64) -> BelResult<Self> {
        let mut heap = Heap::new(heap_capacity);
        let symbols = SymbolTable::new();
        let chars = CharTable::new(&mut heap)?;
        let streams = StreamTable::new();
        let globe = globals::build_globals(&mut heap, &symbols, chars.chars_list)?;

        // smark = (join) — a unique empty pair
        let smark_id = heap.alloc(BelValue::Nil, BelValue::Nil)?;
        let smark = BelValue::Pair(smark_id);

        // vmark = (join) — a unique empty pair
        let vmark_id = heap.alloc(BelValue::Nil, BelValue::Nil)?;
        let vmark = BelValue::Pair(vmark_id);

        // forms = ((smark . <evmark handler>)) — initially just smark dispatch
        // We'll represent evmark as a special tag since it's Rust-native
        let forms = BelValue::Nil; // Will be populated in init_forms

        // prims = ((id join xar xdr wrb ops) (car cdr type sym nom rdb cls stat sys) (coin))
        let prims = BelValue::Nil; // Not directly needed; arity is checked in primitives.rs

        let virfns = BelValue::Nil;
        let locfns = BelValue::Nil;

        let mut m = Machine {
            heap,
            symbols,
            chars,
            streams,
            globe,
            forms,
            prims,
            smark,
            vmark,
            virfns,
            locfns,
            fuel: 0,
            max_fuel,
            interrupted: Arc::new(AtomicBool::new(false)),
            trace: false,
            native_arithmetic: false,
        };

        m.init_forms()?;
        m.init_chars_global()?;
        Ok(m)
    }

    /// Install the `chars` global under the proper name.
    fn init_chars_global(&mut self) -> BelResult<()> {
        let chars_sym = self.symbols.intern("chars");
        let chars_val = self.chars.chars_list;
        env_set(
            BelValue::Symbol(chars_sym),
            chars_val,
            &mut self.globe,
            &mut self.heap,
        )
    }

    /// Install the initial forms dispatch table.
    /// Forms is an alist: ((name . handler) ...)
    /// For Rust-native forms, we store the handler as a special marker:
    ///   (lit prim <form-name>)
    /// The evaluator will check for this pattern and dispatch to Rust code.
    fn init_forms(&mut self) -> BelResult<()> {
        // smark -> evmark (handled specially in the evaluator)
        let smark_entry = self.heap.alloc(self.smark, BelValue::Nil)?;
        let forms_id = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;
        self.forms = BelValue::Pair(forms_id);

        // Install the 7 axiomatic native forms per the Bel spec:
        // quote, if, where, dyn, after, ccc, thread
        // (set is a bootstrap form, replaced by bel.bel's macro at line 990)
        let native_forms = [
            sym::QUOTE, sym::IF, sym::WHERE, sym::DYN,
            sym::AFTER, sym::CCC, sym::THREAD,
        ];

        for &name in &native_forms {
            let marker = globals::make_prim_lit(&mut self.heap, name)?;
            self.put_form(BelValue::Symbol(name), marker)?;
        }

        Ok(())
    }

    /// Add a form entry to the forms alist.
    fn put_form(&mut self, name: BelValue, handler: BelValue) -> BelResult<()> {
        let entry = self.heap.alloc(name, handler)?;
        let new_forms = self.heap.alloc(BelValue::Pair(entry), self.forms)?;
        self.forms = BelValue::Pair(new_forms);
        Ok(())
    }

    /// Get the interrupt flag for use with Ctrl+C handler.
    pub fn interrupt_flag(&self) -> Arc<AtomicBool> {
        self.interrupted.clone()
    }

    // ========================================================================
    // Core evaluation entry point
    // ========================================================================

    /// Evaluate an expression in the global environment.
    /// This is the Rust equivalent of Bel's (bel e).
    pub fn eval(&mut self, expr: BelValue) -> BelResult<BelValue> {
        self.fuel = 0;

        // s = ((e nil)) — expression stack with empty local env
        let ea = self.heap.alloc(expr, BelValue::Nil)?;
        let ea_wrapped = self.heap.alloc(BelValue::Pair(ea), BelValue::Nil)?;
        let s = BelValue::Pair(ea_wrapped);

        // r = nil — empty return stack
        let r = BelValue::Nil;

        // m = (p g) where p = nil, g = globe
        // Built as (nil . (globe . nil))
        let g_node = self.heap.alloc(self.globe, BelValue::Nil)?;
        let m_val = self.heap.alloc(BelValue::Nil, BelValue::Pair(g_node))?;

        self.run_mev(s, r, BelValue::Pair(m_val))
    }

    // ========================================================================
    // The main evaluation loop: mev -> sched -> ev
    // This is an iterative translation of the mutually recursive bel.bel
    // functions. We use an explicit loop with state variables instead of
    // recursive calls to avoid stack overflow.
    // ========================================================================

    /// The main evaluation loop.
    /// mev checks if we're done, sched picks the next thread, ev dispatches.
    fn run_mev(&mut self, mut s: BelValue, mut r: BelValue, mut m: BelValue) -> BelResult<BelValue> {
        loop {
            self.fuel += 1;
            if self.fuel > self.max_fuel {
                return Err(BelError::FuelExhausted);
            }
            if self.interrupted.load(Ordering::Relaxed) {
                return Err(BelError::Interrupted);
            }

            // GC check
            if self.heap.should_gc() {
                self.run_gc(s, r, m);
            }

            // mev (s r (p g))
            if s.is_nil() {
                // No more expressions to evaluate
                let p = self.heap.car_val(m)?;
                if p.is_nil() {
                    // No other threads, return top of return stack
                    return self.heap.car_val(r);
                } else {
                    // Schedule another thread: sched (((s r) . p) g)
                    // p = ((s2 r2) . rest)
                    let sr_pair = self.heap.car_val(p)?;
                    let rest_p = self.heap.cdr_val(p)?;
                    let s2 = self.heap.car_val(sr_pair)?;
                    let r2 = self.heap.cdr_val(sr_pair)?;
                    // Build new m = (rest_p g)
                    let g = self.cadr(m)?;
                    let new_m = self.make_list2(rest_p, g)?;
                    s = s2;
                    r = r2;
                    m = new_m;
                    continue;
                }
            }

            // Check if lock is bound — if so, push this thread to front of p,
            // otherwise to back. For now, simplified: always run current thread.
            // The full lock behavior will be added when bel.bel defines it.

            // sched: pick first thread from s and run ev
            // ev ((e a) . s) r m
            let top = self.heap.car_val(s)?;
            let s_rest = self.heap.cdr_val(s)?;

            // top should be (e . a) where e is expression, a is local env
            let e = self.heap.car_val(top)?;
            let a = self.heap.cdr_val(top)?;

            // Dispatch ev
            let EvalResult::Continue { s: ns, r: nr, m: nm } =
                self.ev_dispatch(e, a, s_rest, r, m)?;
            s = ns;
            r = nr;
            m = nm;
        }
    }

    /// The ev dispatch: determine what kind of expression we have and handle it.
    fn ev_dispatch(
        &mut self,
        e: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // literal: quote, or (lit ...) — return as-is
        if self.is_literal(e)? {
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(e, r)?,
                m,
            });
        }

        // variable: a symbol (not nil, not t, not o, not a quoted/lit thing)
        if self.is_variable(e) {
            return self.vref(e, a, s, r, m);
        }

        // Must be a pair (function call or special form)
        if !e.is_pair() {
            return self.sigerr(sym::MALFORMED, s, r, m);
        }

        // Check if it's not a proper list
        if !self.heap.is_proper_list(e) {
            return self.sigerr(sym::MALFORMED, s, r, m);
        }

        // Check if (car e) is in forms
        let car_e = self.heap.car_val(e)?;
        if let Some(handler) = self.lookup_form(car_e) {
            let args = self.heap.cdr_val(e)?;
            return self.dispatch_form(car_e, handler, args, a, s, r, m);
        }

        // Otherwise: function call — evcall
        self.evcall(e, a, s, r, m)
    }

    // ========================================================================
    // Literals and variables
    // ========================================================================

    /// Check if an expression is a literal (self-evaluating).
    /// In Bel: t, nil, o, characters, streams, (lit ...) forms, strings.
    /// Note: `apply` is NOT a literal — it evaluates to its global value (lit prim apply).
    fn is_literal(&self, e: BelValue) -> BelResult<bool> {
        match e {
            BelValue::Nil | BelValue::Char(_) | BelValue::Stream(_) => Ok(true),
            BelValue::Symbol(id) => {
                Ok(id == sym::T || id == sym::O)
            }
            BelValue::Pair(id) => {
                let car = self.heap.car(id);
                if car == BelValue::Symbol(sym::LIT) {
                    return Ok(true);
                }
                // Check for string (proper list of all chars)
                Ok(self.is_string(e))
            }
        }
    }

    /// Check if an expression is a variable reference.
    /// Variables are symbols other than nil, t, and o.
    fn is_variable(&self, e: BelValue) -> bool {
        match e {
            BelValue::Symbol(id) => {
                id != sym::NIL && id != sym::T && id != sym::O
            }
            // Uvars: (vmark . something) — unique variable created by uvar
            // Check against the global vmark value (set by bel.bel's (set vmark (join)))
            BelValue::Pair(id) => {
                let car = self.heap.car(id);
                // Look up current vmark in globals
                let vmark_name = BelValue::Symbol(
                    self.symbols.lookup("vmark").unwrap_or(sym::NIL)
                );
                if let Some(binding) = globals::env_lookup(vmark_name, self.globe, &self.heap) {
                    if let BelValue::Pair(bid) = binding {
                        let vmark_val = self.heap.cdr(bid);
                        return car == vmark_val;
                    }
                }
                // Fallback: check machine's vmark
                car == self.vmark
            }
            _ => false,
        }
    }

    /// Check if a value is a proper list of all characters (a string).
    fn is_string(&self, val: BelValue) -> bool {
        let mut current = val;
        let mut has_chars = false;
        loop {
            match current {
                BelValue::Nil => return has_chars,
                BelValue::Pair(id) => {
                    let car = self.heap.car(id);
                    if !car.is_char() {
                        return false;
                    }
                    has_chars = true;
                    current = self.heap.cdr(id);
                }
                _ => return false,
            }
        }
    }

    /// Variable reference: look up in local env, dynamic bindings, then global env.
    fn vref(
        &mut self,
        v: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let g = self.cadr(m)?;

        // Check if we're inside a `where` form
        if let Some(inwhere_val) = self.inwhere(s)? {
            // In where context: if variable is found, return (cell . 'd) for settability
            if let Some(cell) = self.lookup(v, a, s, g)? {
                let result = self.make_list2(cell, BelValue::Symbol(sym::D))?;
                let new_s = self.heap.cdr_val(s)?;
                return Ok(EvalResult::Continue {
                    s: new_s,
                    r: self.cons_onto(result, r)?,
                    m,
                });
            }
            // If inwhere allows creation (first element of inwhere val is non-nil)
            if !inwhere_val.is_nil() {
                // Create a new binding cell and add to globe
                let cell = self.heap.alloc(v, BelValue::Nil)?;
                let cell_val = BelValue::Pair(cell);
                // Append to globe
                let g_cdr = self.heap.cdr_val(g)?;
                let new_g_entry = self.heap.alloc(cell_val, g_cdr)?;
                // g is a pair — mutate its cdr
                if let BelValue::Pair(g_id) = g {
                    self.heap.set_cdr(g_id, BelValue::Pair(new_g_entry));
                }
                let result = self.make_list2(cell_val, BelValue::Symbol(sym::D))?;
                let new_s = self.heap.cdr_val(s)?;
                return Ok(EvalResult::Continue {
                    s: new_s,
                    r: self.cons_onto(result, r)?,
                    m,
                });
            }
            return self.sigerr(sym::UNBOUND, s, r, m);
        }

        // Normal variable lookup
        if let Some(cell) = self.lookup(v, a, s, g)? {
            let val = self.heap.cdr_val(cell)?;
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(val, r)?,
                m,
            });
        }

        // Unbound variable
        let err_val = self.make_list2(BelValue::Symbol(sym::UNBOUNDB), v)?;
        self.sigerr_val(err_val, s, r, m)
    }

    /// Look up a variable in all scopes.
    fn lookup(
        &mut self,
        v: BelValue,
        a: BelValue,
        s: BelValue,
        g: BelValue,
    ) -> BelResult<Option<BelValue>> {
        // Check dynamic bindings on the stack
        if let Some(cell) = self.binding(v, s)? {
            return Ok(Some(cell));
        }

        // Check local environment
        if let Some(cell) = env_lookup(v, a, &self.heap) {
            return Ok(Some(cell));
        }

        // Check global environment
        if let Some(cell) = env_lookup(v, g, &self.heap) {
            return Ok(Some(cell));
        }

        // Special cases: scope and globe
        if let BelValue::Symbol(id) = v {
            if id == sym::SCOPE {
                let cell = self.heap.alloc(v, a)?;
                return Ok(Some(BelValue::Pair(cell)));
            }
            if id == sym::GLOBE {
                let cell = self.heap.alloc(v, g)?;
                return Ok(Some(BelValue::Pair(cell)));
            }
        }

        Ok(None)
    }

    /// Look up a dynamic binding on the expression stack.
    /// Scans s for (smark bind (var . val)) entries.
    fn binding(&self, v: BelValue, s: BelValue) -> BelResult<Option<BelValue>> {
        let mut current = s;
        while let BelValue::Pair(id) = current {
            let frame = self.heap.car(id);
            if let BelValue::Pair(fid) = frame {
                let frame_car = self.heap.car(fid);
                // Check if this is a smark entry: ((smark tag ...) env)
                if let BelValue::Pair(entry_id) = frame_car {
                    let entry_car = self.heap.car(entry_id);
                    if entry_car == self.smark {
                        // Check if it's a bind entry
                        let tag = self.heap.cdr(entry_id);
                        if let BelValue::Pair(tag_id) = tag {
                            let tag_sym = self.heap.car(tag_id);
                            if tag_sym == BelValue::Symbol(sym::BIND) {
                                // Get the binding: (cdr tag) = ((var . val) ...)
                                let binding_pair = self.heap.cdr(tag_id);
                                if let BelValue::Pair(bp_id) = binding_pair {
                                    let cell = self.heap.car(bp_id);
                                    if let BelValue::Pair(cell_id) = cell {
                                        if self.heap.car(cell_id) == v {
                                            return Ok(Some(cell));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            current = self.heap.cdr(id);
        }
        Ok(None)
    }

    /// Check if we're inside a `where` form by scanning the stack.
    /// Returns Some(new-flag) if in where, None if not.
    fn inwhere(&self, s: BelValue) -> BelResult<Option<BelValue>> {
        // Check if top of s is ((smark loc ...) env)
        if let BelValue::Pair(id) = s {
            let frame = self.heap.car(id);
            if let BelValue::Pair(fid) = frame {
                let entry = self.heap.car(fid);
                if let BelValue::Pair(eid) = entry {
                    let entry_car = self.heap.car(eid);
                    if entry_car == self.smark {
                        let rest = self.heap.cdr(eid);
                        if let BelValue::Pair(rid) = rest {
                            let tag = self.heap.car(rid);
                            if tag == BelValue::Symbol(sym::LOC) {
                                // Return the cddr — the `new` flag
                                let new_flag = self.heap.cdr(rid);
                                return Ok(Some(new_flag));
                            }
                        }
                    }
                }
            }
        }
        Ok(None)
    }

    // ========================================================================
    // Special forms dispatch
    // ========================================================================

    /// Look up a form in the forms alist.
    fn lookup_form(&self, name: BelValue) -> Option<BelValue> {
        let mut current = self.forms;
        while let BelValue::Pair(id) = current {
            let entry = self.heap.car(id);
            if let BelValue::Pair(eid) = entry {
                let key = self.heap.car(eid);
                if key == name {
                    return Some(self.heap.cdr(eid));
                }
            }
            current = self.heap.cdr(id);
        }
        None
    }

    /// Remove a form from the forms table (when user redefines it as macro/fn).
    fn remove_form(&mut self, name: BelValue) {
        // Build a new forms list excluding the named entry
        let mut entries = Vec::new();
        let mut current = self.forms;
        while let BelValue::Pair(id) = current {
            let entry = self.heap.car(id);
            let mut skip = false;
            if let BelValue::Pair(eid) = entry {
                if self.heap.car(eid) == name {
                    skip = true;
                }
            }
            if !skip {
                entries.push(entry);
            }
            current = self.heap.cdr(id);
        }
        // Rebuild the list
        let mut new_forms = BelValue::Nil;
        for entry in entries.into_iter().rev() {
            if let Ok(pair) = self.heap.alloc(entry, new_forms) {
                new_forms = BelValue::Pair(pair);
            }
        }
        self.forms = new_forms;
    }

    /// Check if a value is a closure: (lit clo ...)
    fn is_clo(&self, v: BelValue) -> bool {
        if let BelValue::Pair(id) = v {
            let car = self.heap.car(id);
            if car == BelValue::Symbol(sym::LIT) {
                let cdr = self.heap.cdr(id);
                if let BelValue::Pair(cdr_id) = cdr {
                    let cadr = self.heap.car(cdr_id);
                    return cadr == BelValue::Symbol(sym::CLO);
                }
            }
        }
        false
    }

    /// Dispatch a special form.
    fn dispatch_form(
        &mut self,
        name: BelValue,
        handler: BelValue,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // Check if it's the smark handler (evmark)
        if name == self.smark {
            return self.evmark(args, a, s, r, m);
        }

        // Check if it's a native form (lit prim <name>)
        if let Some(form_name) = self.is_native_form(handler) {
            return self.dispatch_native_form(form_name, args, a, s, r, m);
        }

        // Otherwise it's a Bel-defined form handler (a closure).
        // Call it with (args . (a s r m)) — the form receives the form arguments
        // plus the machine state.
        // In bel.bel, the form handler signature is (args a s r m).
        // We build the argument list and apply the closure.
        let full_args = self.make_list5(args, a, s, r, m)?;
        self.apply_closure_value(handler, full_args, a, s, r, m)
    }

    /// Check if a handler is a native form marker: (lit prim <name>)
    fn is_native_form(&self, handler: BelValue) -> Option<SymbolId> {
        if let BelValue::Pair(id) = handler {
            let car = self.heap.car(id);
            if car == BelValue::Symbol(sym::LIT) {
                let cdr = self.heap.cdr(id);
                if let BelValue::Pair(cdr_id) = cdr {
                    let cadr = self.heap.car(cdr_id);
                    if cadr == BelValue::Symbol(sym::PRIM) {
                        let cddr = self.heap.cdr(cdr_id);
                        if let BelValue::Pair(cddr_id) = cddr {
                            if let BelValue::Symbol(name) = self.heap.car(cddr_id) {
                                return Some(name);
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Dispatch to a Rust-native special form implementation.
    fn dispatch_native_form(
        &mut self,
        name: SymbolId,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        if name == sym::QUOTE {
            self.form_quote(args, s, r, m)
        } else if name == sym::IF {
            self.form_if(args, a, s, r, m)
        } else if name == sym::WHERE {
            self.form_where(args, a, s, r, m)
        } else if name == sym::DYN {
            self.form_dyn(args, a, s, r, m)
        } else if name == sym::AFTER {
            self.form_after(args, a, s, r, m)
        } else if name == sym::CCC {
            self.form_ccc(args, a, s, r, m)
        } else if name == sym::THREAD {
            self.form_thread(args, a, s, r, m)
        } else if name == sym::DEF || name == sym::FN
            || name == sym::MAC || name == sym::DO || name == sym::LET
            || name == sym::SET
        {
            self.dispatch_bootstrap_form(name, args, a, s, r, m)
        } else {
            self.sigerr(sym::BAD_FORM, s, r, m)
        }
    }

    /// (quote e) — return e unevaluated
    fn form_quote(
        &mut self,
        args: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let e = self.heap.car_val(args)?;
        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(e, r)?,
            m,
        })
    }

    /// (if test then ...) — evaluate test, dispatch on result
    fn form_if(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        if args.is_nil() {
            // (if) -> nil
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(BelValue::Nil, r)?,
                m,
            });
        }

        let test = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;

        // Push test onto s for evaluation
        let test_frame = self.heap.alloc(test, a)?;

        if rest.is_nil() {
            // Only one expr: (if test) — evaluate test, return its value
            let new_s = self.heap.alloc(BelValue::Pair(test_frame), s)?;
            return Ok(EvalResult::Continue {
                s: BelValue::Pair(new_s),
                r,
                m,
            });
        }

        // Push a fut frame that will handle the if2 continuation
        let fut_frame = self.make_if_fut(rest, a)?;
        let s_with_fut = self.heap.alloc(fut_frame, s)?;
        let new_s = self.heap.alloc(BelValue::Pair(test_frame), BelValue::Pair(s_with_fut))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// Build a fut frame for if2: ((smark fut <closure>) nil)
    /// The closure captures the remaining if branches and env.
    fn make_if_fut(&mut self, rest: BelValue, a: BelValue) -> BelResult<BelValue> {
        // Store (if-cont rest a) as a tagged marker
        // We use a special representation: (smark fut (lit prim if) rest a)
        let if_sym = BelValue::Symbol(sym::IF);
        let data = self.heap.alloc(a, BelValue::Nil)?;
        let data = self.heap.alloc(rest, BelValue::Pair(data))?;
        let data = self.heap.alloc(if_sym, BelValue::Pair(data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(data))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;
        Ok(BelValue::Pair(frame))
    }

    /// (where e [new]) — evaluate e in a settable location context
    fn form_where(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let e = self.heap.car_val(args)?;
        let new_flag = self.heap.cdr_val(args)?;
        let new_val = self.heap.car_val(new_flag).unwrap_or(BelValue::Nil);

        // Push loc frame then expression
        let loc_entry = self.heap.alloc(BelValue::Symbol(sym::LOC), BelValue::Nil)?;
        let loc_entry = self.heap.alloc(self.smark, BelValue::Pair(loc_entry))?;
        // Add new_flag
        if !new_val.is_nil() {
            // (smark loc new)
            let loc_rest2 = self.heap.alloc(new_val, BelValue::Nil)?;
            // Actually: (smark . (loc . (new . nil)))
            let loc_tag = self.heap.alloc(BelValue::Symbol(sym::LOC), BelValue::Pair(loc_rest2))?;
            let loc_smark = self.heap.alloc(self.smark, BelValue::Pair(loc_tag))?;
            let loc_frame = self.heap.alloc(BelValue::Pair(loc_smark), BelValue::Nil)?;
            let s_with_loc = self.heap.alloc(BelValue::Pair(loc_frame), s)?;
            let e_frame = self.heap.alloc(e, a)?;
            let new_s = self.heap.alloc(BelValue::Pair(e_frame), BelValue::Pair(s_with_loc))?;
            return Ok(EvalResult::Continue {
                s: BelValue::Pair(new_s),
                r,
                m,
            });
        }

        let loc_frame = self.heap.alloc(BelValue::Pair(loc_entry), BelValue::Nil)?;
        let s_with_loc = self.heap.alloc(BelValue::Pair(loc_frame), s)?;
        let e_frame = self.heap.alloc(e, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(e_frame), BelValue::Pair(s_with_loc))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// (set target1 val1 target2 val2 ...) — assignment
    /// Native form that handles both simple variables and place expressions.
    /// For simple variables, uses lookup to check dynamic → local → global bindings.
    /// For place expressions, uses the where mechanism.
    fn form_set(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        if args.is_nil() {
            // (set) with no args returns nil
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(BelValue::Nil, r)?,
                m,
            });
        }

        let target = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        // Default value is t if no expression given (per bel.bel set macro: (o e t))
        let expr = if rest.is_nil() {
            BelValue::Symbol(sym::T)
        } else {
            self.heap.car_val(rest)?
        };
        let remaining = if rest.is_nil() {
            BelValue::Nil
        } else {
            self.heap.cdr_val(rest)?
        };

        // Build fut data: (native-set target remaining a)
        let native_set_sym = self.symbols.intern("native-set");
        let set_data = self.heap.list(&[
            BelValue::Symbol(native_set_sym),
            target,
            remaining,
            a,
        ])?;

        // Push fut frame, then evaluate expr
        self.push_fut_and_eval(set_data, expr, a, s, r, m)
    }

    /// Handle the native-set continuation after value has been evaluated.
    fn handle_native_set_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data = (native-set target remaining a)
        let rest = self.heap.cdr_val(data)?;
        let target = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let remaining = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let a = self.heap.car_val(rest3)?;

        // Pop the evaluated value from r
        let val = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        if self.is_variable(target) {
            // Simple variable assignment
            let g = self.cadr(m)?;
            if let Some(cell) = self.lookup(target, a, s, g)? {
                // Found a binding — mutate its cdr
                if let BelValue::Pair(cell_id) = cell {
                    self.heap.set_cdr(cell_id, val);
                }
            } else {
                // Not found — create new global binding
                env_set(target, val, &mut self.globe, &mut self.heap)?;
            }

            // Chain to next pair if there are more
            if remaining.is_pair() {
                return self.form_set(remaining, a, s, r_rest, m);
            }

            Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(val, r_rest)?,
                m,
            })
        } else {
            // Place expression — use where mechanism
            // We need to evaluate (where target t) to get (cell . loc),
            // then dispatch on loc to xar or xdr the cell.
            // Store val, remaining, a in a fut for the where result.
            let native_set_where_sym = self.symbols.intern("native-set-where");
            let where_data = self.heap.list(&[
                BelValue::Symbol(native_set_where_sym),
                val,
                remaining,
                a,
            ])?;

            // Push a fut frame for after where completes, then push where evaluation
            let where_args = self.heap.list(&[target, BelValue::Symbol(sym::T)])?;
            // First push the native-set-where fut on s
            let fut_tag = self.make_fut_tag(where_data)?;
            let fut_frame = self.heap.alloc(fut_tag, BelValue::Nil)?;
            let s_with_fut = self.heap.alloc(BelValue::Pair(fut_frame), s)?;

            // Then call form_where which pushes the loc frame and evaluates target
            self.form_where(where_args, a, BelValue::Pair(s_with_fut), r_rest, m)
        }
    }

    /// Handle the native-set-where continuation after where has returned (cell . loc).
    fn handle_native_set_where_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data = (native-set-where val remaining a)
        let rest = self.heap.cdr_val(data)?;
        let val = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let remaining = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let a = self.heap.car_val(rest3)?;

        // Pop the where result from r: (cell loc)
        let where_result = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        // where_result is (cell loc) — a two-element list
        let cell = self.heap.car_val(where_result)?;
        let rest_wr = self.heap.cdr_val(where_result)?;
        let loc = self.heap.car_val(rest_wr)?;

        // Dispatch on loc: a -> xar, d -> xdr
        if let BelValue::Pair(cell_id) = cell {
            if loc == BelValue::Symbol(sym::A) {
                self.heap.set_car(cell_id, val);
            } else {
                // loc == 'd' or anything else defaults to xdr
                self.heap.set_cdr(cell_id, val);
            }
        }

        // Chain to next pair if there are more
        if remaining.is_pair() {
            return self.form_set(remaining, a, s, r_rest, m);
        }

        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(val, r_rest)?,
            m,
        })
    }

    /// (dyn var e1 e2) — dynamic binding
    fn form_dyn(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let v = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let e1 = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let e2 = self.heap.car_val(rest2)?;

        if !self.is_variable(v) {
            return self.sigerr(sym::CANNOT_BIND, s, r, m);
        }

        // Evaluate e1, then bind v to result and evaluate e2
        // Push: fut for dyn2, then e1
        let dyn_data = self.heap.alloc(a, BelValue::Nil)?;
        let dyn_data = self.heap.alloc(e2, BelValue::Pair(dyn_data))?;
        let dyn_data = self.heap.alloc(v, BelValue::Pair(dyn_data))?;
        let dyn_tag = self.heap.alloc(BelValue::Symbol(sym::DYN), BelValue::Pair(dyn_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(dyn_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let fut_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_fut = self.heap.alloc(BelValue::Pair(fut_frame), s)?;
        let e1_frame = self.heap.alloc(e1, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(e1_frame), BelValue::Pair(s_with_fut))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// (after e1 e2) — evaluate e1, then e2 (for cleanup)
    fn form_after(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let e1 = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let e2 = self.heap.car_val(rest)?;

        // Push prot frame, then e1
        // prot entry: (smark . (prot . (e2 . nil)))
        let prot_inner = self.heap.alloc(e2, BelValue::Nil)?;
        let prot_mid = self.heap.alloc(BelValue::Symbol(sym::PROT), BelValue::Pair(prot_inner))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(prot_mid))?;
        let prot_frame = self.heap.alloc(BelValue::Pair(smark_entry), a)?;

        let s_with_prot = self.heap.alloc(BelValue::Pair(prot_frame), s)?;
        let e1_frame = self.heap.alloc(e1, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(e1_frame), BelValue::Pair(s_with_prot))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// (ccc f) — call with current continuation
    fn form_ccc(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let f = self.heap.car_val(args)?;

        // Build continuation: (lit cont s r)
        let cont_r = self.heap.alloc(r, BelValue::Nil)?;
        let cont_s = self.heap.alloc(s, BelValue::Pair(cont_r))?;
        let cont_tag = self.heap.alloc(BelValue::Symbol(sym::CONT), BelValue::Pair(cont_s))?;
        let cont = self.heap.alloc(BelValue::Symbol(sym::LIT), BelValue::Pair(cont_tag))?;

        // Evaluate (f cont) — build the call expression
        let cont_quoted = self.make_list2(BelValue::Symbol(sym::QUOTE), BelValue::Pair(cont))?;
        let call = self.make_list2(f, cont_quoted)?;
        let call_frame = self.heap.alloc(call, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(call_frame), s)?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// (thread e) — create a new thread
    fn form_thread(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let e = self.heap.car_val(args)?;

        // Create new thread: ((e a) . nil) as its stack, nil as its return
        let new_frame = self.heap.alloc(e, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(new_frame), BelValue::Nil)?;
        let new_thread = self.heap.alloc(BelValue::Pair(new_s), BelValue::Nil)?;

        // Add to p
        let p = self.heap.car_val(m)?;
        let g = self.cadr(m)?;
        let new_p = self.heap.alloc(BelValue::Pair(new_thread), p)?;
        let new_m = self.make_list2(BelValue::Pair(new_p), g)?;

        // Current thread returns nil
        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(BelValue::Nil, r)?,
            m: new_m,
        })
    }

    // ========================================================================
    // evmark — handle smark entries on the stack
    // ========================================================================

    /// Handle a smark-tagged stack entry.
    /// The entry format is ((smark tag ...) env).
    fn evmark(
        &mut self,
        entry_cdr: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // entry_cdr is the cdr of the smark entry: (tag . data)
        let tag = self.heap.car_val(entry_cdr)?;

        if tag == BelValue::Symbol(sym::FUT) {
            // fut: the data is a continuation closure or a native marker
            let data = self.heap.cdr_val(entry_cdr)?;
            return self.handle_fut(data, a, s, r, m);
        }

        if tag == BelValue::Symbol(sym::BIND) {
            // bind: skip it, continue with mev
            return Ok(EvalResult::Continue { s, r, m });
        }

        if tag == BelValue::Symbol(sym::LOC) {
            // loc: error — should not be dispatched directly
            return self.sigerr(sym::UNFINDABLE, s, r, m);
        }

        if tag == BelValue::Symbol(sym::PROT) {
            // prot: evaluate the protected expression, then continue
            let e2 = self.heap.cdr_val(entry_cdr)?;
            let e2 = self.heap.car_val(e2)?;

            // Push e2 for evaluation, then a fut that drops its result
            let drop_fut = self.make_drop_fut()?;
            let s_with_drop = self.heap.alloc(drop_fut, s)?;
            let e2_frame = self.heap.alloc(e2, a)?;
            let new_s = self.heap.alloc(BelValue::Pair(e2_frame), BelValue::Pair(s_with_drop))?;

            return Ok(EvalResult::Continue {
                s: BelValue::Pair(new_s),
                r,
                m,
            });
        }

        self.sigerr(sym::UNKNOWN_MARK, s, r, m)
    }

    /// Handle a fut (continuation) frame.
    fn handle_fut(
        &mut self,
        data: BelValue,
        _a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data could be:
        // 1. A native marker like (lit prim if) with extra data — native continuation
        // 2. A Bel closure — call it with (s r m)

        // Check for native continuation markers
        if let BelValue::Pair(id) = data {
            let car = self.heap.car(id);

            // Check if it's a (lit prim <name>) native marker
            if car == BelValue::Symbol(sym::LIT) {
                // This might be our native fut handler, but actually we
                // encode them differently: (lit prim if rest a) etc.
                // Let's check for our specific tags
            }

            // Check for our internal tags
            if let BelValue::Symbol(tag_id) = car {
                if tag_id == sym::IF {
                    return self.handle_if_fut(data, s, r, m);
                }
                if tag_id == sym::DYN {
                    return self.handle_dyn_fut(data, s, r, m);
                }
                if tag_id == sym::PROT {
                    return self.handle_prot_drop_fut(s, r, m);
                }
                if tag_id == sym::APPLY {
                    return self.handle_apply_fut(data, s, r, m);
                }
                // Check string-based custom tags (bootstrap, collect-args, etc.)
                let tag_name = self.symbols.name(tag_id).to_string();
                match tag_name.as_str() {
                    "collect-args" => return self.handle_collect_args_fut(data, s, r, m),
                    "eval-macro-result" => return self.handle_eval_macro_result(data, s, r, m),
                    "eval-body" => return self.handle_eval_body(data, s, r, m),
                    "typecheck" => return self.handle_typecheck_fut(data, s, r, m),
                    "pass-default" => return self.handle_pass_default_fut(data, s, r, m),
                    "destructure-rest" => return self.handle_destructure_rest_fut(data, s, r, m),
                    "native-set" => return self.handle_native_set_fut(data, s, r, m),
                    "native-set-where" => return self.handle_native_set_where_fut(data, s, r, m),
                    "bootstrap-set" => return self.handle_bootstrap_set_fut(data, s, r, m),
                    "bootstrap-do" => return self.handle_bootstrap_do_fut(data, s, r, m),
                    _ => {}
                }
            }
        }

        // It's a Bel closure — apply it with (s r m)
        // This is used when bel.bel defines fu closures
        let args = self.make_list3(s, r, m)?;
        self.apply_value(data, args, s, r, m)
    }

    /// Handle the if2 continuation.
    fn handle_if_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data = (if rest a)
        let rest_and_a = self.heap.cdr_val(data)?;
        let rest = self.heap.car_val(rest_and_a)?;
        let a_list = self.heap.cdr_val(rest_and_a)?;
        let a = self.heap.car_val(a_list)?;

        // Pop the test result from r
        let test_result = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        if !test_result.is_nil() {
            // Test passed: evaluate the first branch
            let then_expr = self.heap.car_val(rest)?;
            let then_frame = self.heap.alloc(then_expr, a)?;
            let new_s = self.heap.alloc(BelValue::Pair(then_frame), s)?;
            Ok(EvalResult::Continue {
                s: BelValue::Pair(new_s),
                r: r_rest,
                m,
            })
        } else {
            // Test failed: evaluate (if rest-of-branches)
            let else_branches = self.heap.cdr_val(rest)?;
            if else_branches.is_nil() {
                // No more branches: return nil
                Ok(EvalResult::Continue {
                    s,
                    r: self.cons_onto(BelValue::Nil, r_rest)?,
                    m,
                })
            } else {
                // Build (if else-branches...) and evaluate
                let if_expr = self.heap.alloc(BelValue::Symbol(sym::IF), else_branches)?;
                let frame = self.heap.alloc(BelValue::Pair(if_expr), a)?;
                let new_s = self.heap.alloc(BelValue::Pair(frame), s)?;
                Ok(EvalResult::Continue {
                    s: BelValue::Pair(new_s),
                    r: r_rest,
                    m,
                })
            }
        }
    }

    /// Handle the dyn2 continuation.
    fn handle_dyn_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data = (dyn v e2 a)
        let rest = self.heap.cdr_val(data)?;
        let v = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let e2 = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let a = self.heap.car_val(rest3)?;

        // Pop the evaluated value of e1 from r
        let val = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        // Push bind frame with (v . val), then e2
        let binding = self.heap.alloc(v, val)?;
        let bind_data = self.heap.alloc(BelValue::Pair(binding), BelValue::Nil)?;
        let bind_tag = self.heap.alloc(BelValue::Symbol(sym::BIND), BelValue::Pair(bind_data))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(bind_tag))?;
        let bind_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_bind = self.heap.alloc(BelValue::Pair(bind_frame), s)?;
        let e2_frame = self.heap.alloc(e2, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(e2_frame), BelValue::Pair(s_with_bind))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r: r_rest,
            m,
        })
    }

    /// Handle prot drop continuation: discard the result of the protected expr.
    fn handle_prot_drop_fut(
        &mut self,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // Drop the top of r (the result of the protected expression)
        let r_rest = self.heap.cdr_val(r)?;
        Ok(EvalResult::Continue { s, r: r_rest, m })
    }

    /// Build a fut frame that drops its result (for prot cleanup).
    fn make_drop_fut(&mut self) -> BelResult<BelValue> {
        let prot_tag = BelValue::Symbol(sym::PROT);
        let data = self.heap.alloc(prot_tag, BelValue::Nil)?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(data))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;
        Ok(BelValue::Pair(frame))
    }

    // ========================================================================
    // Function calls: evcall, evcall2, applyf chain
    // ========================================================================

    /// evcall: evaluate the operator, then the arguments, then apply.
    fn evcall(
        &mut self,
        e: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let op_expr = self.heap.car_val(e)?;
        let arg_exprs = self.heap.cdr_val(e)?;

        // Push fut for evcall2, then evaluate operator
        let evcall2_data = self.heap.alloc(a, BelValue::Nil)?;
        let evcall2_data = self.heap.alloc(arg_exprs, BelValue::Pair(evcall2_data))?;
        let evcall2_tag = self.heap.alloc(BelValue::Symbol(sym::APPLY), BelValue::Pair(evcall2_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(evcall2_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let fut_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_fut = self.heap.alloc(BelValue::Pair(fut_frame), s)?;
        let op_frame = self.heap.alloc(op_expr, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(op_frame), BelValue::Pair(s_with_fut))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// Handle the evcall2 continuation: operator has been evaluated.
    fn handle_apply_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data = (apply arg_exprs a)
        let rest = self.heap.cdr_val(data)?;
        let arg_exprs = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let a = self.heap.car_val(rest2)?;

        // Pop operator from r
        let op = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        // Check if operator is a macro
        if self.is_mac(op)? {
            return self.applym(op, arg_exprs, a, s, r_rest, m);
        }

        // Evaluate all arguments, then apply
        // Count arguments
        let arg_count = self.list_len(arg_exprs)?;

        if arg_count == 0 {
            // No arguments — apply directly
            return self.applyf(op, BelValue::Nil, a, s, r_rest, m);
        }

        // Push frames to evaluate each argument, then a fut to collect and apply
        // We need to evaluate arg_exprs left-to-right, collect results, then apply
        // Strategy: push all arg evals + a final fut that snaps results from r

        // Build the apply-after fut
        let apply_data = self.heap.alloc(a, BelValue::Nil)?;
        let apply_data = self.heap.alloc(op, BelValue::Pair(apply_data))?;
        // Store arg count so we can snap that many values from r
        let count_val = self.make_unary_int(arg_count)?;
        let apply_data = self.heap.alloc(count_val, BelValue::Pair(apply_data))?;
        let collect_sym = self.symbols.intern("collect-args");
        let apply_tag = self.heap.alloc(BelValue::Symbol(collect_sym), BelValue::Pair(apply_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(apply_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let collect_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        // Build argument evaluation frames (in reverse so they eval left-to-right)
        let mut new_s = self.heap.alloc(BelValue::Pair(collect_frame), s)?;
        let args_vec = self.list_to_vec(arg_exprs)?;
        for arg_expr in args_vec.iter().rev() {
            let frame = self.heap.alloc(*arg_expr, a)?;
            new_s = self.heap.alloc(BelValue::Pair(frame), BelValue::Pair(new_s))?;
        }

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r: r_rest,
            m,
        })
    }

    /// Handle collect-args fut: pop N values from r, reverse them, apply.
    fn handle_collect_args_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data = (collect-args count op a)
        let rest = self.heap.cdr_val(data)?;
        let count_val = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let op = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let a = self.heap.car_val(rest3)?;

        let count = self.unary_to_usize(count_val);

        // Pop count values from r, they were pushed last-first so reverse
        let mut args = Vec::new();
        let mut current_r = r;
        for _ in 0..count {
            let val = self.heap.car_val(current_r)?;
            args.push(val);
            current_r = self.heap.cdr_val(current_r)?;
        }
        args.reverse();

        let args_list = self.heap.list(&args)?;
        self.applyf(op, args_list, a, s, current_r, m)
    }

    /// Apply a macro: call the macro's inner closure with unevaluated args,
    /// then evaluate the result.
    fn applym(
        &mut self,
        mac: BelValue,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // mac = (lit mac (lit clo ...))
        // Get the inner closure: (caddr mac)
        let mac_clo = self.caddr(mac)?;

        // Push a fut that evaluates the macro expansion result
        let eval_fut_data = self.heap.alloc(a, BelValue::Nil)?;
        let eval_sym = self.symbols.intern("eval-macro-result");
        let eval_tag = self.heap.alloc(BelValue::Symbol(eval_sym), BelValue::Pair(eval_fut_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(eval_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let eval_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_eval = self.heap.alloc(BelValue::Pair(eval_frame), s)?;

        // Apply the macro closure to the unevaluated args
        self.applyf(mac_clo, args, a, BelValue::Pair(s_with_eval), r, m)
    }

    /// Apply a function value to a list of evaluated arguments.
    fn applyf(
        &mut self,
        f: BelValue,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // Fast-path: native number arithmetic
        if self.native_arithmetic {
            if let Some(result) = self.try_native_arithmetic(f, args)? {
                return Ok(EvalResult::Continue {
                    s,
                    r: self.cons_onto(result, r)?,
                    m,
                });
            }
        }

        // Check for the special `apply` function: (lit prim apply)
        if self.is_prim_named(f, sym::APPLY) {
            // (apply f args...) -> flatten args and apply f
            let inner_f = self.heap.car_val(args)?;
            let rest_args = self.heap.cdr_val(args)?;
            let flat_args = self.reduce_join(rest_args)?;
            return self.applyf(inner_f, flat_args, a, s, r, m);
        }

        // Must be (lit ...)
        if !self.caris(f, sym::LIT)? {
            return self.sigerr(sym::CANNOT_APPLY, s, r, m);
        }

        // Check if it's a proper list
        if !self.heap.is_proper_list(f) {
            return self.sigerr(sym::BAD_LIT, s, r, m);
        }

        self.applylit(f, args, a, s, r, m)
    }

    /// Apply a lit-tagged value.
    fn applylit(
        &mut self,
        f: BelValue,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // Check if we're inside a `where` form and there's a matching locfn.
        // In bel.bel: (aif (and (inwhere s) (find [(car _) f] locfns)) ...)
        // We handle the common locfn cases natively:
        // - (lit prim car) → return (arg . a) meaning: cell is arg, location is car
        // - (lit prim cdr) → return (arg . d) meaning: cell is arg, location is cdr
        if self.inwhere(s)?.is_some() {
            if let Some(result) = self.native_locfn(f, args)? {
                // Pop the loc frame from s
                let s_rest = self.heap.cdr_val(s)?;
                return Ok(EvalResult::Continue {
                    s: s_rest,
                    r: self.cons_onto(result, r)?,
                    m,
                });
            }
        }

        // f = (lit tag . rest)
        let tag_and_rest = self.heap.cdr_val(f)?;
        let tag = self.heap.car_val(tag_and_rest)?;
        let rest = self.heap.cdr_val(tag_and_rest)?;

        if let BelValue::Symbol(tag_id) = tag {
            if tag_id == sym::PRIM {
                let prim_name = self.heap.car_val(rest)?;
                return self.applyprim(prim_name, args, s, r, m);
            }

            if tag_id == sym::CLO {
                return self.applyclo(rest, args, s, r, m);
            }

            if tag_id == sym::MAC {
                // Applying a macro as a function: quote all args first
                let quoted_args = self.quote_all(args)?;
                return self.applym(f, quoted_args, a, s, r, m);
            }

            if tag_id == sym::CONT {
                return self.applycont(rest, args, s, r, m);
            }
        }

        // Check virfns (virtual functions) — not implemented yet
        self.sigerr(sym::UNAPPLYABLE, s, r, m)
    }

    /// Apply a primitive.
    fn applyprim(
        &mut self,
        prim_name: BelValue,
        args: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let name_id = match prim_name {
            BelValue::Symbol(id) => id,
            _ => return self.sigerr(sym::UNKNOWN_PRIM, s, r, m),
        };

        // Extract args
        let a = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let b = self.heap.car_val(rest)?;
        let nargs = self.list_len(args)?;

        match primitives::call_primitive(
            name_id,
            a,
            b,
            nargs,
            &mut self.heap,
            &mut self.symbols,
            &mut self.streams,
        ) {
            Ok(val) => {
                Ok(EvalResult::Continue {
                    s,
                    r: self.cons_onto(val, r)?,
                    m,
                })
            }
            Err(e) => {
                // Primitive signaled an error — convert to sigerr
                let err_sym = match e {
                    BelError::OverArgs => sym::OVERARGS,
                    BelError::TypeError(_) => sym::MISTYPE,
                    _ => sym::NO_ERR,
                };
                self.sigerr(err_sym, s, r, m)
            }
        }
    }

    /// Apply a closure: (lit clo env parms body)
    fn applyclo(
        &mut self,
        rest: BelValue,
        args: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // rest = (env parms body . extra)
        let env = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let parms = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let body = self.heap.car_val(rest3)?;

        if self.trace {
            let p = crate::printer::print_val(parms, &self.heap, &self.symbols);
            let a = crate::printer::print_val(args, &self.heap, &self.symbols);
            let b = crate::printer::print_val(body, &self.heap, &self.symbols);
            eprintln!("[applyclo] parms={} args={} body={}", p, a, b);
        }

        // Phase 1: pass (bind parameters) — pushes a fut for phase 2 (eval body)
        // Build body eval fut
        let body_data = self.heap.alloc(body, BelValue::Nil)?;
        let body_sym = self.symbols.intern("eval-body");
        let body_tag = self.heap.alloc(BelValue::Symbol(body_sym), BelValue::Pair(body_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(body_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let body_fut = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;
        let s_with_body = self.heap.alloc(BelValue::Pair(body_fut), s)?;

        // Now do parameter binding
        self.pass(parms, args, env, BelValue::Pair(s_with_body), r, m)
    }

    /// Parameter binding: match parms against args, building up env.
    fn pass(
        &mut self,
        pat: BelValue,
        arg: BelValue,
        env: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        if self.trace {
            let pat_s = crate::printer::print_val(pat, &self.heap, &self.symbols);
            let arg_s = crate::printer::print_val(arg, &self.heap, &self.symbols);
            eprintln!("[pass] pat={} arg={} is_var={} is_lit={:?}", pat_s, arg_s, self.is_variable(pat), self.is_literal(pat));
        }
        // No more parameters
        if pat.is_nil() {
            if !arg.is_nil() {
                return self.sigerr(sym::OVERARGS, s, r, m);
            }
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(env, r)?,
                m,
            });
        }

        // Literal parameter: error
        if self.is_literal(pat)? {
            return self.sigerr(sym::LITERAL_PARM, s, r, m);
        }

        // Simple variable: bind it
        if self.is_variable(pat) {
            let new_env = self.heap.alloc(pat, arg)?;
            let new_env = self.heap.alloc(BelValue::Pair(new_env), env)?;
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(BelValue::Pair(new_env), r)?,
                m,
            });
        }

        // (t var type-check) — typed parameter
        if self.caris(pat, sym::T)? {
            let rest = self.heap.cdr_val(pat)?;
            let var = self.heap.car_val(rest)?;
            let rest2 = self.heap.cdr_val(rest)?;
            let type_fn = self.heap.car_val(rest2)?;

            // Evaluate (type_fn arg), if true then bind var
            let quoted_arg = self.make_list2(BelValue::Symbol(sym::QUOTE), arg)?;
            let check_expr = self.make_list2(type_fn, quoted_arg)?;
            let check_frame = self.heap.alloc(check_expr, env)?;

            // Push fut to handle typecheck result
            let tc_data = self.heap.alloc(env, BelValue::Nil)?;
            let tc_data = self.heap.alloc(arg, BelValue::Pair(tc_data))?;
            let tc_data = self.heap.alloc(var, BelValue::Pair(tc_data))?;
            let tc_sym = self.symbols.intern("typecheck");
            let tc_tag = self.heap.alloc(BelValue::Symbol(tc_sym), BelValue::Pair(tc_data))?;
            let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(tc_tag))?;
            let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
            let tc_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

            let s_with_tc = self.heap.alloc(BelValue::Pair(tc_frame), s)?;
            let new_s = self.heap.alloc(BelValue::Pair(check_frame), BelValue::Pair(s_with_tc))?;

            return Ok(EvalResult::Continue {
                s: BelValue::Pair(new_s),
                r,
                m,
            });
        }

        // (o var default) — optional parameter
        if self.caris(pat, sym::O)? {
            let rest = self.heap.cdr_val(pat)?;
            let var = self.heap.car_val(rest)?;
            return self.pass(var, arg, env, s, r, m);
        }

        // Destructuring: (p . ps) against (car arg . cdr arg)
        self.destructure(pat, arg, env, s, r, m)
    }

    /// Destructure a pair pattern against an argument.
    fn destructure(
        &mut self,
        pat: BelValue,
        arg: BelValue,
        env: BelValue,
        s: BelValue,

        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let p = self.heap.car_val(pat)?;
        let ps = self.heap.cdr_val(pat)?;

        if arg.is_nil() {
            // No more args: check for optional
            if self.caris(p, sym::O)? {
                // Evaluate default, bind, then continue with ps
                let rest = self.heap.cdr_val(p)?;
                let var = self.heap.car_val(rest)?;
                let rest2 = self.heap.cdr_val(rest)?;
                let default_expr = self.heap.car_val(rest2)?;

                if default_expr.is_nil() {
                    // No default: bind to nil
                    return self.pass(var, BelValue::Nil, env, s, r, m);
                }

                // Evaluate default expression
                let default_frame = self.heap.alloc(default_expr, env)?;

                // Push fut for pass-after-default
                let pd_data = self.heap.alloc(env, BelValue::Nil)?;
                let pd_data = self.heap.alloc(ps, BelValue::Pair(pd_data))?;
                let pd_data = self.heap.alloc(var, BelValue::Pair(pd_data))?;
                let pd_sym = self.symbols.intern("pass-default");
                let pd_tag = self.heap.alloc(BelValue::Symbol(pd_sym), BelValue::Pair(pd_data))?;
                let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(pd_tag))?;
                let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
                let pd_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

                let s_with_pd = self.heap.alloc(BelValue::Pair(pd_frame), s)?;
                let new_s = self.heap.alloc(BelValue::Pair(default_frame), BelValue::Pair(s_with_pd))?;

                return Ok(EvalResult::Continue {
                    s: BelValue::Pair(new_s),
                    r,
                    m,
                });
            }
            return self.sigerr(sym::UNDERARGS, s, r, m);
        }

        if arg.is_atom() {
            return self.sigerr(sym::ATOM_ARG, s, r, m);
        }

        // Destructure: match p against (car arg), then ps against (cdr arg)
        let car_arg = self.heap.car_val(arg)?;
        let cdr_arg = self.heap.cdr_val(arg)?;

        // We need to pass p against (car arg), get new env, then pass ps against (cdr arg)
        // Push fut for second pass
        let ds_data = self.heap.alloc(cdr_arg, BelValue::Nil)?;
        let ds_data = self.heap.alloc(ps, BelValue::Pair(ds_data))?;
        let ds_sym = self.symbols.intern("destructure-rest");
        let ds_tag = self.heap.alloc(BelValue::Symbol(ds_sym), BelValue::Pair(ds_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(ds_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let ds_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_ds = self.heap.alloc(BelValue::Pair(ds_frame), s)?;

        self.pass(p, car_arg, env, BelValue::Pair(s_with_ds), r, m)
    }

    /// Apply a continuation: (lit cont s2 r2)
    fn applycont(
        &mut self,
        rest: BelValue,
        args: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let nargs = self.list_len(args)?;
        if nargs != 1 {
            return self.sigerr(sym::WRONG_NO_ARGS, s, r, m);
        }

        let val = self.heap.car_val(args)?;
        let s2 = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let r2 = self.heap.car_val(rest2)?;

        // Restore continuation: prepend protected frames from current s
        let protected_frames = self.collect_protected(s, s2)?;
        let new_s = self.append(protected_frames, s2)?;

        Ok(EvalResult::Continue {
            s: new_s,
            r: self.cons_onto(val, r2)?,
            m,
        })
    }

    /// Collect protected (after) frames from s that aren't in s2.
    fn collect_protected(&self, s: BelValue, _s2: BelValue) -> BelResult<BelValue> {
        let result = BelValue::Nil;
        let mut current = s;
        while let BelValue::Pair(id) = current {
            let frame = self.heap.car(id);
            if self.is_protected_frame(frame) {
                // Would check if frame is in s2, simplified for now
                let _ = frame; // TODO: full protected frame handling
            }
            current = self.heap.cdr(id);
        }
        Ok(result)
    }

    /// Check if a frame is a prot (after) frame.
    fn is_protected_frame(&self, frame: BelValue) -> bool {
        if let BelValue::Pair(fid) = frame {
            let entry = self.heap.car(fid);
            if let BelValue::Pair(eid) = entry {
                let car = self.heap.car(eid);
                if car == self.smark {
                    let rest = self.heap.cdr(eid);
                    if let BelValue::Pair(rid) = rest {
                        let tag = self.heap.car(rid);
                        return tag == BelValue::Symbol(sym::PROT);
                    }
                }
            }
        }
        false
    }

    // ========================================================================
    // Bootstrap fut handlers
    // ========================================================================

    /// Handle bootstrap-set fut: set global variable to evaluated value
    fn handle_bootstrap_set_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // data is (bootstrap-set var remaining)
        let rest = self.heap.cdr_val(data)?;
        let var = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let remaining = self.heap.car_val(rest2)?;

        let val = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        env_set(var, val, &mut self.globe, &mut self.heap)?;

        // If a bootstrap form name is being redefined (e.g., as a macro),
        // remove it from the forms table so the new definition takes effect.
        if self.lookup_form(var).is_some() {
            if self.is_mac(val).unwrap_or(false) || self.is_clo(val) {
                self.remove_form(var);
            }
        }

        // If there are more var-value pairs, chain them
        if remaining.is_pair() {
            return self.bootstrap_set(remaining, BelValue::Nil, s, r_rest, m);
        }

        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(val, r_rest)?,
            m,
        })
    }

    /// Handle bootstrap-do fut: drop result, evaluate rest
    fn handle_bootstrap_do_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let rest = self.heap.cdr_val(data)?;
        let exprs = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let a = self.heap.car_val(rest2)?;

        // Drop the top of r (previous do expression result)
        let r_rest = self.heap.cdr_val(r)?;

        // Evaluate remaining do expressions
        self.bootstrap_do(exprs, a, s, r_rest, m)
    }

    // ========================================================================
    // Error signaling
    // ========================================================================

    /// Signal a Bel-level error.
    fn sigerr(
        &mut self,
        err_sym: SymbolId,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let msg = BelValue::Symbol(err_sym);
        self.sigerr_val(msg, s, r, m)
    }

    /// Signal a Bel-level error with a value.
    /// Searches the stack for a dynamic binding of `err`.
    /// If found, calls the handler with the error message.
    /// If not found, returns a Rust-level error.
    fn sigerr_val(
        &mut self,
        err_val: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        // Search stack s for a bind entry containing `err`
        if let Some(handler) = self.find_dynamic_binding(sym::ERR, s) {
            // Call handler with (err_val) as args
            let args = self.heap.alloc(err_val, BelValue::Nil)?;
            return self.applyf(handler, BelValue::Pair(args), BelValue::Nil, s, r, m);
        }
        // No handler found — return a Rust error
        let name = crate::printer::print_val(err_val, &self.heap, &self.symbols);
        Err(BelError::BelErr(name))
    }

    /// Search the expression stack for a dynamic binding of the given variable.
    /// Dynamic bindings are stored as smark/bind entries on the stack.
    /// Returns the bound value if found.
    fn find_dynamic_binding(&self, var: SymbolId, s: BelValue) -> Option<BelValue> {
        let var_val = BelValue::Symbol(var);
        let mut current = s;
        while let BelValue::Pair(id) = current {
            let frame = self.heap.car(id);
            // Each frame is (expr . env). Check if expr starts with (smark bind ...)
            if let BelValue::Pair(fid) = frame {
                let expr = self.heap.car(fid);
                if let BelValue::Pair(eid) = expr {
                    let e_car = self.heap.car(eid);
                    if e_car == self.smark {
                        // It's an smark entry. Check tag type.
                        let e_cdr = self.heap.cdr(eid);
                        if let BelValue::Pair(tag_id) = e_cdr {
                            let tag_car = self.heap.car(tag_id);
                            if tag_car == BelValue::Symbol(sym::BIND) {
                                // It's a bind entry. Check if it binds our variable.
                                let tag_cdr = self.heap.cdr(tag_id);
                                if let BelValue::Pair(bind_data_id) = tag_cdr {
                                    let binding = self.heap.car(bind_data_id);
                                    if let BelValue::Pair(bid) = binding {
                                        let bvar = self.heap.car(bid);
                                        if bvar == var_val {
                                            return Some(self.heap.cdr(bid));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            current = self.heap.cdr(id);
        }
        None
    }

    // ========================================================================
    // Apply a Bel value as a function (used for form handlers and futs)
    // ========================================================================

    fn apply_value(
        &mut self,
        f: BelValue,
        args: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        self.applyf(f, args, BelValue::Nil, s, r, m)
    }

    fn apply_closure_value(
        &mut self,
        f: BelValue,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        self.applyf(f, args, a, s, r, m)
    }

    /// Handle eval-macro-result: evaluate the macro expansion
    fn handle_eval_macro_result(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let rest = self.heap.cdr_val(data)?;
        let a = self.heap.car_val(rest)?;

        let result = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        let frame = self.heap.alloc(result, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(frame), s)?;
        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r: r_rest,
            m,
        })
    }

    /// Handle eval-body: evaluate closure body with bound env
    fn handle_eval_body(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let rest = self.heap.cdr_val(data)?;
        let body = self.heap.car_val(rest)?;

        let env = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        let frame = self.heap.alloc(body, env)?;
        let new_s = self.heap.alloc(BelValue::Pair(frame), s)?;
        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r: r_rest,
            m,
        })
    }

    /// Handle typecheck fut: check if type check passed
    fn handle_typecheck_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let rest = self.heap.cdr_val(data)?;
        let var = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let arg = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let env = self.heap.car_val(rest3)?;

        let check_result = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        if !check_result.is_nil() {
            self.pass(var, arg, env, s, r_rest, m)
        } else {
            self.sigerr(sym::MISTYPE, s, r_rest, m)
        }
    }

    /// Handle pass-default fut: bind var to default value, continue with ps
    fn handle_pass_default_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let rest = self.heap.cdr_val(data)?;
        let var = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let ps = self.heap.car_val(rest2)?;
        let rest3 = self.heap.cdr_val(rest2)?;
        let env = self.heap.car_val(rest3)?;

        let default_val = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        // Bind var to default_val in env
        let binding = self.heap.alloc(var, default_val)?;
        let new_env = self.heap.alloc(BelValue::Pair(binding), env)?;

        // Continue with ps (rest of params) against nil
        self.pass(ps, BelValue::Nil, BelValue::Pair(new_env), s, r_rest, m)
    }

    /// Handle destructure-rest fut: continue destructuring with updated env
    fn handle_destructure_rest_fut(
        &mut self,
        data: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let rest = self.heap.cdr_val(data)?;
        let ps = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let cdr_arg = self.heap.car_val(rest2)?;

        let env = self.heap.car_val(r)?;
        let r_rest = self.heap.cdr_val(r)?;

        self.pass(ps, cdr_arg, env, s, r_rest, m)
    }

    // ========================================================================
    // GC
    // ========================================================================

    fn run_gc(&mut self, s: BelValue, r: BelValue, m: BelValue) {
        self.heap.clear_marks();
        let mut worklist = Vec::new();

        // Mark all roots
        self.heap.mark_value(s, &mut worklist);
        self.heap.mark_value(r, &mut worklist);
        self.heap.mark_value(m, &mut worklist);
        self.heap.mark_value(self.globe, &mut worklist);
        self.heap.mark_value(self.forms, &mut worklist);
        self.heap.mark_value(self.smark, &mut worklist);
        self.heap.mark_value(self.vmark, &mut worklist);
        self.heap.mark_value(self.chars.chars_list, &mut worklist);
        self.heap.mark_value(self.virfns, &mut worklist);
        self.heap.mark_value(self.locfns, &mut worklist);
        self.heap.mark_value(self.prims, &mut worklist);

        self.heap.process_worklist(&mut worklist);

        self.heap.sweep();
        self.heap.reset_gc_counter();
        self.heap.adjust_gc_threshold();
    }

    // ========================================================================
    // Helper methods
    // ========================================================================

    /// Check if a value is a macro: (lit mac ...)
    fn is_mac(&self, v: BelValue) -> BelResult<bool> {
        if let BelValue::Pair(id) = v {
            let car = self.heap.car(id);
            if car == BelValue::Symbol(sym::LIT) {
                let cdr = self.heap.cdr(id);
                if let BelValue::Pair(cdr_id) = cdr {
                    let cadr = self.heap.car(cdr_id);
                    return Ok(cadr == BelValue::Symbol(sym::MAC));
                }
            }
        }
        Ok(false)
    }

    /// Check if a value is (lit prim name)
    fn is_prim_named(&self, v: BelValue, name: SymbolId) -> bool {
        if let BelValue::Pair(id) = v {
            let car = self.heap.car(id);
            if car == BelValue::Symbol(sym::LIT) {
                let cdr = self.heap.cdr(id);
                if let BelValue::Pair(cdr_id) = cdr {
                    let cadr = self.heap.car(cdr_id);
                    if cadr == BelValue::Symbol(sym::PRIM) {
                        let cddr = self.heap.cdr(cdr_id);
                        if let BelValue::Pair(cddr_id) = cddr {
                            let caddr = self.heap.car(cddr_id);
                            return caddr == BelValue::Symbol(name);
                        }
                    }
                }
            }
        }
        false
    }

    /// Check if (car x) == sym
    fn caris(&self, x: BelValue, sym: SymbolId) -> BelResult<bool> {
        match x {
            BelValue::Pair(id) => Ok(self.heap.car(id) == BelValue::Symbol(sym)),
            _ => Ok(false),
        }
    }

    /// Get cadr of a value.
    fn cadr(&self, v: BelValue) -> BelResult<BelValue> {
        let cdr = self.heap.cdr_val(v)?;
        self.heap.car_val(cdr)
    }

    /// Get caddr of a value.
    fn caddr(&self, v: BelValue) -> BelResult<BelValue> {
        let cdr = self.heap.cdr_val(v)?;
        let cddr = self.heap.cdr_val(cdr)?;
        self.heap.car_val(cddr)
    }

    /// Cons a value onto a list.
    fn cons_onto(&mut self, val: BelValue, list: BelValue) -> BelResult<BelValue> {
        let pair = self.heap.alloc(val, list)?;
        Ok(BelValue::Pair(pair))
    }

    /// Build a list of 2 elements.
    fn make_list2(&mut self, a: BelValue, b: BelValue) -> BelResult<BelValue> {
        self.heap.list(&[a, b])
    }

    /// Build a list of 3 elements.
    fn make_list3(&mut self, a: BelValue, b: BelValue, c: BelValue) -> BelResult<BelValue> {
        self.heap.list(&[a, b, c])
    }

    /// Build a list of 5 elements.
    fn make_list5(
        &mut self,
        a: BelValue,
        b: BelValue,
        c: BelValue,
        d: BelValue,
        e: BelValue,
    ) -> BelResult<BelValue> {
        self.heap.list(&[a, b, c, d, e])
    }

    /// Handle locfn behavior natively for common cases.
    /// Returns Some(result) if f matches a known location function, None otherwise.
    /// For car: returns (arg . a) — the arg pair, settable via car
    /// For cdr: returns (arg . d) — the arg pair, settable via cdr
    fn native_locfn(&mut self, f: BelValue, args: BelValue) -> BelResult<Option<BelValue>> {
        // Check if f is (lit prim car) or (lit prim cdr)
        if let BelValue::Pair(f_id) = f {
            let f_car = self.heap.car(f_id);
            if f_car == BelValue::Symbol(sym::LIT) {
                let f_cdr = self.heap.cdr(f_id);
                if let BelValue::Pair(fc_id) = f_cdr {
                    let tag = self.heap.car(fc_id);
                    if tag == BelValue::Symbol(sym::PRIM) {
                        let name_rest = self.heap.cdr(fc_id);
                        if let BelValue::Pair(nr_id) = name_rest {
                            let prim_name = self.heap.car(nr_id);
                            if let BelValue::Symbol(prim_id) = prim_name {
                                let arg = self.heap.car_val(args)?;
                                if prim_id == sym::CAR {
                                    // (car x) → location is (x . a)
                                    let result = self.make_list2(arg, BelValue::Symbol(sym::A))?;
                                    return Ok(Some(result));
                                }
                                if prim_id == sym::CDR {
                                    // (cdr x) → location is (x . d)
                                    let result = self.make_list2(arg, BelValue::Symbol(sym::D))?;
                                    return Ok(Some(result));
                                }
                            }
                        }
                    }
                }
            }
        }

        // Also check locfns table for user-defined location functions
        // (e.g., for cadr, caddr, nth, table access, etc.)
        let mut current = self.locfns;
        while let BelValue::Pair(id) = current {
            let entry = self.heap.car(id);
            if let BelValue::Pair(entry_id) = entry {
                let test = self.heap.car(entry_id);
                // Try to match the test closure's captured value against f
                if self.locfn_test_matches(test, f)? {
                    let rest = self.heap.cdr(entry_id);
                    if let BelValue::Pair(r) = rest {
                        let handler = self.heap.car(r);
                        // For now, return None for user-defined locfns
                        // (they require calling Bel closures which manipulate s/r directly)
                        // TODO: handle user-defined locfns natively
                        let _ = handler;
                    }
                }
            }
            current = self.heap.cdr(id);
        }

        Ok(None)
    }

    /// Check if a locfn test closure matches a function value.
    fn locfn_test_matches(&self, test: BelValue, f: BelValue) -> BelResult<bool> {
        // Common pattern: (lit clo ((x . <prim-value>)) (_) (= _ x))
        // Extract the captured value and compare
        if let BelValue::Pair(test_id) = test {
            let test_car = self.heap.car(test_id);
            if test_car == BelValue::Symbol(sym::LIT) {
                let test_cdr = self.heap.cdr(test_id);
                if let BelValue::Pair(tc_id) = test_cdr {
                    let tag = self.heap.car(tc_id);
                    if tag == BelValue::Symbol(sym::CLO) {
                        let rest = self.heap.cdr(tc_id);
                        if let BelValue::Pair(rest_id) = rest {
                            let env = self.heap.car(rest_id);
                            if let BelValue::Pair(env_id) = env {
                                let first_binding = self.heap.car(env_id);
                                if let BelValue::Pair(fb_id) = first_binding {
                                    let captured_val = self.heap.cdr(fb_id);
                                    return self.bel_equal(f, captured_val);
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(false)
    }

    /// Check structural equality of two Bel values (like bel's = function).
    fn bel_equal(&self, a: BelValue, b: BelValue) -> BelResult<bool> {
        if a == b {
            return Ok(true);
        }
        match (a, b) {
            (BelValue::Pair(aid), BelValue::Pair(bid)) => {
                let a_car = self.heap.car(aid);
                let b_car = self.heap.car(bid);
                let a_cdr = self.heap.cdr(aid);
                let b_cdr = self.heap.cdr(bid);
                Ok(self.bel_equal(a_car, b_car)? && self.bel_equal(a_cdr, b_cdr)?)
            }
            _ => Ok(false),
        }
    }

    /// Build a fut tag: (smark . (fut . data))
    fn make_fut_tag(&mut self, data: BelValue) -> BelResult<BelValue> {
        let fut_entry = self.heap.alloc(BelValue::Symbol(sym::FUT), data)?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_entry))?;
        Ok(BelValue::Pair(smark_entry))
    }

    /// Push a fut frame on the stack and schedule evaluation of an expression.
    /// This is the common pattern: push ((smark fut data) nil) on s, then (expr a) on top.
    fn push_fut_and_eval(
        &mut self,
        fut_data: BelValue,
        expr: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let fut_tag = self.make_fut_tag(fut_data)?;
        let fut_frame = self.heap.alloc(fut_tag, BelValue::Nil)?;
        let s_with_fut = self.heap.alloc(BelValue::Pair(fut_frame), s)?;
        let expr_frame = self.heap.alloc(expr, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(expr_frame), BelValue::Pair(s_with_fut))?;
        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// Convert list to Vec.
    fn list_to_vec(&self, list: BelValue) -> BelResult<Vec<BelValue>> {
        self.heap.list_to_vec(list).ok_or_else(|| BelError::TypeError("not a proper list".into()))
    }

    /// Length of a proper list.
    fn list_len(&self, list: BelValue) -> BelResult<usize> {
        let mut count = 0;
        let mut current = list;
        loop {
            match current {
                BelValue::Nil => return Ok(count),
                BelValue::Pair(id) => {
                    count += 1;
                    current = self.heap.cdr(id);
                }
                _ => return Err(BelError::TypeError("not a proper list".into())),
            }
        }
    }

    /// Append two lists.
    fn append(&mut self, a: BelValue, b: BelValue) -> BelResult<BelValue> {
        if a.is_nil() {
            return Ok(b);
        }
        let items = self.list_to_vec(a)?;
        let mut result = b;
        for &item in items.iter().rev() {
            let pair = self.heap.alloc(item, result)?;
            result = BelValue::Pair(pair);
        }
        Ok(result)
    }

    /// Quote all elements of a list: (a b c) -> ('a 'b 'c)
    fn quote_all(&mut self, list: BelValue) -> BelResult<BelValue> {
        if list.is_nil() {
            return Ok(BelValue::Nil);
        }
        let items = self.list_to_vec(list)?;
        let mut quoted = Vec::new();
        for item in items {
            let q = self.make_list2(BelValue::Symbol(sym::QUOTE), item)?;
            quoted.push(q);
        }
        self.heap.list(&quoted)
    }

    /// reduce join on a list of lists: flatten one level
    /// Flatten args for `apply`: (a1 a2 ... last) -> (a1 a2 ... . last)
    /// The last element is spread into the list.
    fn reduce_join(&mut self, lists: BelValue) -> BelResult<BelValue> {
        if lists.is_nil() {
            return Ok(BelValue::Nil);
        }
        let items = self.list_to_vec(lists)?;
        if items.is_empty() {
            return Ok(BelValue::Nil);
        }
        if items.len() == 1 {
            // Single arg: spread it directly
            return Ok(items[0]);
        }
        // Prepend all items except the last onto the last item
        let last = *items.last().unwrap();
        let mut result = last;
        for item in items[..items.len() - 1].iter().rev() {
            let pair = self.heap.alloc(*item, result)?;
            result = BelValue::Pair(pair);
        }
        Ok(result)
    }

    /// Encode a small int as a unary list of symbols (for internal use).
    // ========================================================================
    // Native number arithmetic optimization
    // ========================================================================

    /// Extract a rational number (numer, denom) from a Bel number value.
    /// Returns None if the value is not a number or has non-zero imaginary part.
    fn extract_rational(&self, val: BelValue) -> Option<(i64, i64)> {
        // Must be (lit num real imag)
        let id = val.as_pair()?;
        if self.heap.car(id) != BelValue::Symbol(sym::LIT) { return None; }
        let cdr = self.heap.cdr(id).as_pair()?;
        if self.heap.car(cdr) != BelValue::Symbol(sym::NUM) { return None; }
        let cddr = self.heap.cdr(cdr).as_pair()?;
        let real_part = self.heap.car(cddr);
        let cdddr = self.heap.cdr(cddr).as_pair()?;
        let imag_part = self.heap.car(cdddr);

        // Check imaginary part is zero: (+ nil (t))
        let imag_id = imag_part.as_pair()?;
        let imag_numer_rest = self.heap.cdr(imag_id).as_pair()?;
        let imag_numer = self.heap.car(imag_numer_rest);
        if !imag_numer.is_nil() { return None; } // non-zero imaginary

        // Decode real part: (sign numer denom)
        let real_id = real_part.as_pair()?;
        let sign_val = self.heap.car(real_id);
        let rest = self.heap.cdr(real_id).as_pair()?;
        let numer_val = self.heap.car(rest);
        let rest2 = self.heap.cdr(rest).as_pair()?;
        let denom_val = self.heap.car(rest2);

        let sign: i64 = if sign_val == BelValue::Symbol(sym::MINUS) { -1 } else { 1 };
        let numer = self.unary_len(numer_val) as i64;
        let denom = self.unary_len(denom_val) as i64;

        if denom == 0 { return None; }

        Some((sign * numer, denom))
    }

    /// Count length of a unary list (t t t ...).
    fn unary_len(&self, val: BelValue) -> usize {
        let mut count = 0;
        let mut current = val;
        while let BelValue::Pair(id) = current {
            count += 1;
            current = self.heap.cdr(id);
        }
        count
    }

    /// Construct a Bel number from a rational (numer, denom).
    fn make_bel_number(&mut self, numer: i64, denom: i64) -> BelResult<BelValue> {
        let g = gcd(numer.unsigned_abs() as u64, denom.unsigned_abs() as u64);
        let n = numer.unsigned_abs() as u64 / g;
        let d = denom.unsigned_abs() as u64 / g;
        let sign_positive = (numer >= 0) == (denom > 0);

        let sign = if sign_positive {
            BelValue::Symbol(sym::PLUS)
        } else {
            BelValue::Symbol(sym::MINUS)
        };

        let numer_unary = self.make_unary_int(n as usize)?;
        let denom_unary = self.make_unary_int(d as usize)?;

        // Real part: (sign numer denom)
        let real = self.heap.list(&[sign, numer_unary, denom_unary])?;

        // Imaginary part: (+, nil, (t)) = +0/1
        let imag_sign = BelValue::Symbol(sym::PLUS);
        let imag_denom = self.heap.alloc(BelValue::Symbol(sym::T), BelValue::Nil)?;
        let imag = self.heap.list(&[imag_sign, BelValue::Nil, BelValue::Pair(imag_denom)])?;

        // (lit num real imag)
        self.heap.list(&[
            BelValue::Symbol(sym::LIT),
            BelValue::Symbol(sym::NUM),
            real,
            imag,
        ])
    }

    /// Check if a function value is a specific global function by name.
    fn is_global_fn(&self, f: BelValue, name: SymbolId) -> bool {
        let sym_val = BelValue::Symbol(name);
        if let Some(cell) = globals::env_lookup(sym_val, self.globe, &self.heap) {
            if let BelValue::Pair(cid) = cell {
                let val = self.heap.cdr(cid);
                // Check identity — the function value should be the exact same object
                return val == f;
            }
        }
        false
    }

    /// Try to handle arithmetic natively. Returns Some(result) if handled.
    fn try_native_arithmetic(
        &mut self,
        f: BelValue,
        args: BelValue,
    ) -> BelResult<Option<BelValue>> {
        // Quick check: is f a closure? (lit clo ...)
        if !self.is_clo(f) { return Ok(None); }

        // Check which arithmetic function this is
        let is_plus = self.is_global_fn(f, sym::PLUS);
        let is_minus = self.is_global_fn(f, sym::MINUS);
        let is_times = self.is_global_fn(f, sym::TIMES);
        let is_slash = self.is_global_fn(f, sym::SLASH);
        let is_less = self.is_global_fn(f, sym::LESS);

        if !is_plus && !is_minus && !is_times && !is_slash && !is_less {
            return Ok(None);
        }

        // Extract all numeric arguments
        let mut nums: Vec<(i64, i64)> = Vec::new();
        let mut current = args;
        while let BelValue::Pair(id) = current {
            let arg = self.heap.car(id);
            if let Some(rat) = self.extract_rational(arg) {
                nums.push(rat);
            } else {
                return Ok(None); // non-numeric argument, fall through
            }
            current = self.heap.cdr(id);
        }

        if nums.is_empty() {
            // (+ ) -> 0, (* ) -> 1
            if is_plus || is_minus {
                return Ok(Some(self.make_bel_number(0, 1)?));
            }
            if is_times {
                return Ok(Some(self.make_bel_number(1, 1)?));
            }
            return Ok(None);
        }

        if is_plus {
            // (+ a b c ...) = a + b + c + ...
            let mut result = (0i64, 1i64);
            for (n, d) in &nums {
                result = rat_add(result.0, result.1, *n, *d);
            }
            return Ok(Some(self.make_bel_number(result.0, result.1)?));
        }

        if is_minus {
            if nums.len() == 1 {
                // (- a) = -a
                return Ok(Some(self.make_bel_number(-nums[0].0, nums[0].1)?));
            }
            // (- a b c ...) = a - b - c - ...
            let mut result = nums[0];
            for (n, d) in &nums[1..] {
                result = rat_sub(result.0, result.1, *n, *d);
            }
            return Ok(Some(self.make_bel_number(result.0, result.1)?));
        }

        if is_times {
            let mut result = (1i64, 1i64);
            for (n, d) in &nums {
                result = rat_mul(result.0, result.1, *n, *d);
            }
            return Ok(Some(self.make_bel_number(result.0, result.1)?));
        }

        if is_slash {
            if nums.is_empty() { return Ok(None); }
            if nums.len() == 1 {
                // (/ a) = 1/a
                return Ok(Some(self.make_bel_number(nums[0].1, nums[0].0)?));
            }
            // (/ a b c ...) = a / b / c / ...
            let mut result = nums[0];
            for (n, d) in &nums[1..] {
                if *n == 0 { return Ok(None); } // division by zero, let Bel handle
                result = rat_div(result.0, result.1, *n, *d);
            }
            return Ok(Some(self.make_bel_number(result.0, result.1)?));
        }

        if is_less {
            // (< a b c ...) = a < b < c < ... (each consecutive pair)
            if nums.len() < 2 {
                return Ok(Some(BelValue::Symbol(sym::T)));
            }
            for i in 0..nums.len()-1 {
                let (n1, d1) = nums[i];
                let (n2, d2) = nums[i+1];
                // Compare n1/d1 < n2/d2 => n1*d2 < n2*d1
                let lhs = n1.checked_mul(d2);
                let rhs = n2.checked_mul(d1);
                match (lhs, rhs) {
                    (Some(l), Some(r)) => {
                        if l >= r { return Ok(Some(BelValue::Nil)); }
                    }
                    _ => return Ok(None), // overflow, let Bel handle
                }
            }
            return Ok(Some(BelValue::Symbol(sym::T)));
        }

        Ok(None)
    }

    fn make_unary_int(&mut self, n: usize) -> BelResult<BelValue> {
        let mut result = BelValue::Nil;
        for _ in 0..n {
            let pair = self.heap.alloc(BelValue::Symbol(sym::T), result)?;
            result = BelValue::Pair(pair);
        }
        Ok(result)
    }

    /// Decode a unary int back to usize.
    fn unary_to_usize(&self, val: BelValue) -> usize {
        let mut count = 0;
        let mut current = val;
        while let BelValue::Pair(id) = current {
            count += 1;
            current = self.heap.cdr(id);
        }
        count
    }

    /// Set a global variable.
    pub fn set_global(&mut self, name: SymbolId, val: BelValue) -> BelResult<()> {
        env_set(BelValue::Symbol(name), val, &mut self.globe, &mut self.heap)
    }

    /// Install simplified bootstrap forms (set, def, fn, mac, do, let).
    /// These are temporary native handlers, replaced when bel.bel defines its own versions.
    /// Specifically: set is redefined as a macro at bel.bel line 990; the others earlier.
    pub fn install_bootstrap_forms(&mut self) -> BelResult<()> {
        // Install a simplified `set` that just sets globals
        // Install a simplified `def` that creates (lit clo nil parms body) closures
        // Install a simplified `mac` that creates (lit mac (lit clo nil parms body))
        // Install a simplified `fn` that creates (lit clo nil parms body)
        // These are handled as native forms in the evaluator

        // set, def, fn, mac, do, let are bootstrap forms.
        // They are installed here as native handlers and replaced when bel.bel
        // defines its own versions (set at line 990, the others earlier).
        let def_sym = sym::DEF;
        let fn_sym = sym::FN;
        let mac_sym = sym::MAC;
        let do_sym = sym::DO;
        let let_sym = sym::LET;
        let set_sym = sym::SET;

        // Install as native form handlers
        for &name in &[set_sym, def_sym, fn_sym, mac_sym, do_sym, let_sym] {
            let marker = globals::make_prim_lit(&mut self.heap, name)?;
            self.put_form(BelValue::Symbol(name), marker)?;
        }

        Ok(())
    }

    /// Dispatch bootstrap native forms (set, def, fn, mac, do, let).
    fn dispatch_bootstrap_form(
        &mut self,
        name: SymbolId,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        if name == sym::SET {
            return self.bootstrap_set(args, a, s, r, m);
        }
        if name == sym::DEF {
            return self.bootstrap_def(args, a, s, r, m);
        }
        if name == sym::FN {
            return self.bootstrap_fn(args, a, s, r, m);
        }
        if name == sym::MAC {
            return self.bootstrap_mac(args, a, s, r, m);
        }
        if name == sym::DO {
            return self.bootstrap_do(args, a, s, r, m);
        }
        if name == sym::LET {
            return self.bootstrap_let(args, a, s, r, m);
        }
        self.sigerr(sym::BAD_FORM, s, r, m)
    }

    /// Bootstrap (set var expr ...) — evaluate expr, set global, handle multiple pairs
    fn bootstrap_set(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let var = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let expr = self.heap.car_val(rest)?;
        let remaining = self.heap.cdr_val(rest)?;

        // Store remaining pairs in the fut data so handle_bootstrap_set_fut can chain them
        let set_data = self.heap.list(&[var, remaining])?;
        let set_sym = self.symbols.intern("bootstrap-set");
        let set_tag = self.heap.alloc(BelValue::Symbol(set_sym), set_data)?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(set_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let fut_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_fut = self.heap.alloc(BelValue::Pair(fut_frame), s)?;
        let expr_frame = self.heap.alloc(expr, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(expr_frame), BelValue::Pair(s_with_fut))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// Bootstrap (def name parms body) -> (set name (lit clo nil parms body))
    fn bootstrap_def(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let name = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let parms = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let body = self.heap.car_val(rest2)?;

        // Build (lit clo nil parms body) — note: using `a` for scope capture
        let clo = self.make_closure(a, parms, body)?;

        // Set the global
        env_set(name, clo, &mut self.globe, &mut self.heap)?;

        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(name, r)?,
            m,
        })
    }

    /// Bootstrap (fn parms body) -> (lit clo <scope> parms body)
    fn bootstrap_fn(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let parms = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let body = self.heap.car_val(rest)?;

        let clo = self.make_closure(a, parms, body)?;

        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(clo, r)?,
            m,
        })
    }

    /// Bootstrap (mac name parms body) -> (set name (lit mac (lit clo nil parms body)))
    fn bootstrap_mac(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let name = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let parms = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let body = self.heap.car_val(rest2)?;

        let clo = self.make_closure(a, parms, body)?;
        // Wrap in (lit mac clo)
        let mac_inner = self.heap.alloc(clo, BelValue::Nil)?;
        let mac_mid = self.heap.alloc(BelValue::Symbol(sym::MAC), BelValue::Pair(mac_inner))?;
        let mac = self.heap.alloc(BelValue::Symbol(sym::LIT), BelValue::Pair(mac_mid))?;
        let mac_val = BelValue::Pair(mac);

        env_set(name, mac_val, &mut self.globe, &mut self.heap)?;

        Ok(EvalResult::Continue {
            s,
            r: self.cons_onto(name, r)?,
            m,
        })
    }

    /// Bootstrap (do e1 e2 ...) — evaluate each in sequence, return last
    fn bootstrap_do(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        if args.is_nil() {
            return Ok(EvalResult::Continue {
                s,
                r: self.cons_onto(BelValue::Nil, r)?,
                m,
            });
        }

        let first = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;

        if rest.is_nil() {
            // Last expression: just evaluate it
            let frame = self.heap.alloc(first, a)?;
            let new_s = self.heap.alloc(BelValue::Pair(frame), s)?;
            return Ok(EvalResult::Continue {
                s: BelValue::Pair(new_s),
                r,
                m,
            });
        }

        // Evaluate first, drop result, evaluate rest
        let do_data = self.heap.alloc(a, BelValue::Nil)?;
        let do_data = self.heap.alloc(rest, BelValue::Pair(do_data))?;
        let do_sym = self.symbols.intern("bootstrap-do");
        let do_tag = self.heap.alloc(BelValue::Symbol(do_sym), BelValue::Pair(do_data))?;
        let fut_tag = self.heap.alloc(BelValue::Symbol(sym::FUT), BelValue::Pair(do_tag))?;
        let smark_entry = self.heap.alloc(self.smark, BelValue::Pair(fut_tag))?;
        let do_frame = self.heap.alloc(BelValue::Pair(smark_entry), BelValue::Nil)?;

        let s_with_do = self.heap.alloc(BelValue::Pair(do_frame), s)?;
        let first_frame = self.heap.alloc(first, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(first_frame), BelValue::Pair(s_with_do))?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// Bootstrap (let var expr body) — evaluate expr, bind var, evaluate body
    fn bootstrap_let(
        &mut self,
        args: BelValue,
        a: BelValue,
        s: BelValue,
        r: BelValue,
        m: BelValue,
    ) -> BelResult<EvalResult> {
        let var = self.heap.car_val(args)?;
        let rest = self.heap.cdr_val(args)?;
        let expr = self.heap.car_val(rest)?;
        let rest2 = self.heap.cdr_val(rest)?;
        let body = self.heap.car_val(rest2)?;

        // Build a closure: ((fn (var) body) expr)
        // Param must be (var) not var — otherwise pass binds var to the entire args list
        let parm_list = self.heap.alloc(var, BelValue::Nil)?;
        let clo = self.make_closure(a, BelValue::Pair(parm_list), body)?;
        let quoted_clo = self.make_list2(BelValue::Symbol(sym::QUOTE), clo)?;
        let call = self.make_list2(quoted_clo, expr)?;
        let frame = self.heap.alloc(call, a)?;
        let new_s = self.heap.alloc(BelValue::Pair(frame), s)?;

        Ok(EvalResult::Continue {
            s: BelValue::Pair(new_s),
            r,
            m,
        })
    }

    /// Make a closure: (lit clo env parms body)
    fn make_closure(
        &mut self,
        env: BelValue,
        parms: BelValue,
        body: BelValue,
    ) -> BelResult<BelValue> {
        let body_node = self.heap.alloc(body, BelValue::Nil)?;
        let parms_node = self.heap.alloc(parms, BelValue::Pair(body_node))?;
        let env_node = self.heap.alloc(env, BelValue::Pair(parms_node))?;
        let clo_node = self.heap.alloc(BelValue::Symbol(sym::CLO), BelValue::Pair(env_node))?;
        let lit_node = self.heap.alloc(BelValue::Symbol(sym::LIT), BelValue::Pair(clo_node))?;
        Ok(BelValue::Pair(lit_node))
    }
}

/// Result of a single evaluation step.
enum EvalResult {
    /// Continue evaluating with new state.
    Continue {
        s: BelValue,
        r: BelValue,
        m: BelValue,
    },
}

// ============================================================================
// Rational arithmetic helpers (free functions)
// ============================================================================

fn gcd(a: u64, b: u64) -> u64 {
    let mut a = a;
    let mut b = b;
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a.max(1)
}

fn rat_add(n1: i64, d1: i64, n2: i64, d2: i64) -> (i64, i64) {
    let n = n1 * d2 + n2 * d1;
    let d = d1 * d2;
    let g = gcd(n.unsigned_abs(), d.unsigned_abs());
    (n / g as i64, d / g as i64)
}

fn rat_sub(n1: i64, d1: i64, n2: i64, d2: i64) -> (i64, i64) {
    rat_add(n1, d1, -n2, d2)
}

fn rat_mul(n1: i64, d1: i64, n2: i64, d2: i64) -> (i64, i64) {
    let n = n1 * n2;
    let d = d1 * d2;
    let g = gcd(n.unsigned_abs(), d.unsigned_abs());
    (n / g as i64, d / g as i64)
}

fn rat_div(n1: i64, d1: i64, n2: i64, d2: i64) -> (i64, i64) {
    rat_mul(n1, d1, d2, n2)
}
