use crate::error::BelResult;
use crate::heap::Heap;
use crate::symbol::{sym, SymbolTable};
use crate::value::BelValue;

/// Build the initial global environment.
/// The globe is a list of (name . value) pairs.
/// Pre-installs:
///   - t = t
///   - nil = nil
///   - chars = the character table list
///   - ins = nil (stdin)
///   - outs = nil (stdout)
///   - All 16 primitives as (lit prim name)
pub fn build_globals(
    heap: &mut Heap,
    _symbols: &SymbolTable,
    chars_list: BelValue,
) -> BelResult<BelValue> {
    let mut globe = BelValue::Nil;

    // Helper: prepend (name . val) to globe
    macro_rules! def_global {
        ($sym:expr, $val:expr) => {
            let binding = heap.alloc(BelValue::Symbol($sym), $val)?;
            let node = heap.alloc(BelValue::Pair(binding), globe)?;
            globe = BelValue::Pair(node);
        };
    }

    // t = t
    def_global!(sym::T, BelValue::Symbol(sym::T));

    // nil = nil
    def_global!(sym::NIL, BelValue::Nil);

    // o = o (used as optional parameter marker)
    def_global!(sym::O, BelValue::Symbol(sym::O));

    // apply = (lit prim apply) — special: apply is both a prim and a special dispatch
    let apply_lit = make_prim_lit(heap, sym::APPLY)?;
    def_global!(sym::APPLY, apply_lit);

    // chars = the character table
    // chars = the character table (Bel uses the global name "chars")
    def_global!(sym::CHARS, chars_list);

    // ins = nil (stdin is nil)
    def_global!(sym::INS, BelValue::Nil);

    // outs = nil (stdout is nil)
    def_global!(sym::OUTS, BelValue::Nil);

    // Install all 16 primitives as (lit prim name)
    let prim_names = [
        sym::ID, sym::JOIN, sym::CAR, sym::CDR, sym::TYPE,
        sym::XAR, sym::XDR, sym::SYM, sym::NOM,
        sym::WRB, sym::RDB, sym::OPS, sym::CLS, sym::STAT,
        sym::COIN, sym::SYS,
    ];

    for &prim_sym in &prim_names {
        let lit_val = make_prim_lit(heap, prim_sym)?;
        def_global!(prim_sym, lit_val);
    }

    Ok(globe)
}

/// Build (lit prim name) — the standard representation of a primitive function.
pub fn make_prim_lit(heap: &mut Heap, name: crate::value::SymbolId) -> BelResult<BelValue> {
    // (lit prim name) = (lit . (prim . (name . nil)))
    let name_val = BelValue::Symbol(name);
    let inner = heap.alloc(name_val, BelValue::Nil)?;
    let mid = heap.alloc(BelValue::Symbol(sym::PRIM), BelValue::Pair(inner))?;
    let outer = heap.alloc(BelValue::Symbol(sym::LIT), BelValue::Pair(mid))?;
    Ok(BelValue::Pair(outer))
}

/// Look up a binding in an environment (association list).
/// Returns the (name . value) pair if found, or None.
pub fn env_lookup(name: BelValue, env: BelValue, heap: &Heap) -> Option<BelValue> {
    let mut current = env;
    loop {
        match current {
            BelValue::Nil => return None,
            BelValue::Pair(id) => {
                let binding = heap.car(id);
                if let BelValue::Pair(bid) = binding {
                    let bname = heap.car(bid);
                    if bname == name {
                        return Some(binding);
                    }
                }
                current = heap.cdr(id);
            }
            _ => return None,
        }
    }
}

/// Set or add a binding in the global environment.
/// If the name already exists, mutate its value. Otherwise prepend.
pub fn env_set(
    name: BelValue,
    val: BelValue,
    globe: &mut BelValue,
    heap: &mut Heap,
) -> BelResult<()> {
    // First, try to find and mutate existing binding
    let mut current = *globe;
    while let BelValue::Pair(id) = current {
        let binding = heap.car(id);
        if let BelValue::Pair(bid) = binding {
            let bname = heap.car(bid);
            if bname == name {
                heap.set_cdr(bid, val);
                return Ok(());
            }
        }
        current = heap.cdr(id);
    }

    // Not found — prepend new binding
    let binding = heap.alloc(name, val)?;
    let node = heap.alloc(BelValue::Pair(binding), *globe)?;
    *globe = BelValue::Pair(node);
    Ok(())
}
