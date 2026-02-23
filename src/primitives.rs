use crate::chars::CharTable;
use crate::error::{BelError, BelResult};
use crate::heap::Heap;
use crate::stream::StreamTable;
use crate::symbol::{sym, SymbolTable};
use crate::value::{BelValue, CharId, SymbolId};
use rand::Rng;

/// Dispatch a primitive call by its symbol name.
/// `a` and `b` are the first and second arguments (nil if not supplied).
/// `nargs` is the actual number of arguments supplied (for arity checking).
///
/// The arity groups from bel.bel (line 499):
///   2 args: id join xar xdr wrb ops
///   1 arg:  car cdr type sym nom rdb cls stat sys
///   0 args: coin
///
/// Missing args default to nil. Extra args cause overargs error.
pub fn call_primitive(
    name: SymbolId,
    a: BelValue,
    b: BelValue,
    nargs: usize,
    heap: &mut Heap,
    symbols: &mut SymbolTable,
    streams: &mut StreamTable,
) -> BelResult<BelValue> {
    // Arity checking: max args for each primitive
    let max_args = if name == sym::ID
        || name == sym::JOIN
        || name == sym::XAR
        || name == sym::XDR
        || name == sym::WRB
        || name == sym::OPS
    {
        2
    } else if name == sym::CAR
        || name == sym::CDR
        || name == sym::TYPE
        || name == sym::SYM
        || name == sym::NOM
        || name == sym::RDB
        || name == sym::CLS
        || name == sym::STAT
        || name == sym::SYS
    {
        1
    } else if name == sym::COIN {
        0
    } else {
        return Err(BelError::BelErr(format!(
            "unknown primitive: {}",
            symbols.name(name)
        )));
    };

    if nargs > max_args {
        return Err(BelError::OverArgs);
    }

    // Dispatch
    if name == sym::ID {
        prim_id(a, b)
    } else if name == sym::JOIN {
        prim_join(a, b, heap)
    } else if name == sym::CAR {
        prim_car(a, heap)
    } else if name == sym::CDR {
        prim_cdr(a, heap)
    } else if name == sym::TYPE {
        prim_type(a, heap)
    } else if name == sym::XAR {
        prim_xar(a, b, heap)
    } else if name == sym::XDR {
        prim_xdr(a, b, heap)
    } else if name == sym::SYM {
        prim_sym(a, heap, symbols)
    } else if name == sym::NOM {
        prim_nom(a, heap, symbols)
    } else if name == sym::WRB {
        prim_wrb(a, b, streams)
    } else if name == sym::RDB {
        prim_rdb(a, streams)
    } else if name == sym::OPS {
        prim_ops(a, b, heap, symbols, streams)
    } else if name == sym::CLS {
        prim_cls(a, streams)
    } else if name == sym::STAT {
        prim_stat(a, symbols, streams)
    } else if name == sym::COIN {
        prim_coin()
    } else if name == sym::SYS {
        prim_sys(a, heap)
    } else {
        Err(BelError::Internal("unreachable primitive dispatch".into()))
    }
}

/// (id a b) — identity comparison.
/// For pairs: compares PairId (pointer identity, not structural equality).
/// For atoms: value equality.
fn prim_id(a: BelValue, b: BelValue) -> BelResult<BelValue> {
    if a == b {
        Ok(BelValue::Symbol(sym::T))
    } else {
        Ok(BelValue::Nil)
    }
}

/// (join a b) — create a new cons cell.
fn prim_join(a: BelValue, b: BelValue, heap: &mut Heap) -> BelResult<BelValue> {
    let id = heap.alloc(a, b)?;
    Ok(BelValue::Pair(id))
}

/// (car x) — car of pair, nil of nil.
fn prim_car(a: BelValue, heap: &Heap) -> BelResult<BelValue> {
    heap.car_val(a)
}

/// (cdr x) — cdr of pair, nil of nil.
fn prim_cdr(a: BelValue, heap: &Heap) -> BelResult<BelValue> {
    heap.cdr_val(a)
}

/// (type x) — return the type symbol: symbol, pair, char, stream.
/// nil is a symbol.
/// For pairs of the form (lit tag ...) where tag is mac, clo, prim, or cont,
/// returns the tag instead of pair.
fn prim_type(a: BelValue, heap: &Heap) -> BelResult<BelValue> {
    let type_sym = match a {
        BelValue::Nil | BelValue::Symbol(_) => sym::SYMBOL,
        BelValue::Pair(id) => {
            let car = heap.car(id);
            if car == BelValue::Symbol(sym::LIT) {
                let cdr = heap.cdr(id);
                if let BelValue::Pair(cdr_id) = cdr {
                    let tag = heap.car(cdr_id);
                    match tag {
                        BelValue::Symbol(s)
                            if s == sym::MAC
                                || s == sym::CLO
                                || s == sym::PRIM
                                || s == sym::CONT =>
                        {
                            s
                        }
                        _ => sym::PAIR,
                    }
                } else {
                    sym::PAIR
                }
            } else {
                sym::PAIR
            }
        }
        BelValue::Char(_) => sym::CHAR,
        BelValue::Stream(_) => sym::STREAM,
    };
    Ok(BelValue::Symbol(type_sym))
}

/// (xar p v) — set car of pair to v, return v.
fn prim_xar(a: BelValue, b: BelValue, heap: &mut Heap) -> BelResult<BelValue> {
    match a {
        BelValue::Pair(id) => {
            heap.set_car(id, b);
            Ok(b)
        }
        _ => Err(BelError::TypeError("xar: not a pair".into())),
    }
}

/// (xdr p v) — set cdr of pair to v, return v.
fn prim_xdr(a: BelValue, b: BelValue, heap: &mut Heap) -> BelResult<BelValue> {
    match a {
        BelValue::Pair(id) => {
            heap.set_cdr(id, b);
            Ok(b)
        }
        _ => Err(BelError::TypeError("xdr: not a pair".into())),
    }
}

/// (sym x) — convert a character list to a symbol.
/// x must be a proper list of chars. Returns the interned symbol.
fn prim_sym(a: BelValue, heap: &Heap, symbols: &mut SymbolTable) -> BelResult<BelValue> {
    let mut name = String::new();
    let mut current = a;
    loop {
        match current {
            BelValue::Nil => break,
            BelValue::Pair(id) => {
                let ch = heap.car(id);
                match ch {
                    BelValue::Char(cid) => {
                        name.push(CharTable::to_char(cid));
                    }
                    _ => return Err(BelError::TypeError("sym: list element is not a char".into())),
                }
                current = heap.cdr(id);
            }
            _ => return Err(BelError::TypeError("sym: not a proper list".into())),
        }
    }
    let id = symbols.intern(&name);
    Ok(BelValue::Symbol(id))
}

/// (nom s) — return the name of symbol s as a fresh list of characters.
fn prim_nom(a: BelValue, heap: &mut Heap, symbols: &SymbolTable) -> BelResult<BelValue> {
    match a {
        BelValue::Symbol(id) => {
            let name = symbols.name(id);
            let chars: Vec<BelValue> = name
                .bytes()
                .map(|b| {
                    if b < 128 {
                        BelValue::Char(CharId(b))
                    } else {
                        // Non-ASCII: use replacement char
                        BelValue::Char(CharId(b'?'))
                    }
                })
                .collect();
            heap.list(&chars)
        }
        BelValue::Nil => {
            // nil is a symbol in Bel
            let chars: Vec<BelValue> = b"nil"
                .iter()
                .map(|&b| BelValue::Char(CharId(b)))
                .collect();
            heap.list(&chars)
        }
        _ => Err(BelError::TypeError("nom: not a symbol".into())),
    }
}

/// (wrb bit stream) — write a bit (\1 or \0 char) to stream.
/// If stream is nil, write to stdout.
fn prim_wrb(a: BelValue, b: BelValue, streams: &mut StreamTable) -> BelResult<BelValue> {
    let bit = match a {
        BelValue::Char(cid) if cid.0 == b'1' => true,
        BelValue::Char(cid) if cid.0 == b'0' => false,
        _ => return Err(BelError::TypeError("wrb: first arg must be \\1 or \\0".into())),
    };

    let stream_id = match b {
        BelValue::Nil => StreamTable::stdout_id(),
        BelValue::Stream(id) => id,
        _ => return Err(BelError::TypeError("wrb: second arg must be a stream or nil".into())),
    };

    streams.write_bit(stream_id, bit)?;
    Ok(a) // wrb returns the bit that was written
}

/// (rdb stream) — read a bit from stream.
/// Returns \1, \0, or nil (no data yet). On EOF returns eof symbol.
/// If stream is nil, read from stdin.
fn prim_rdb(a: BelValue, streams: &mut StreamTable) -> BelResult<BelValue> {
    let stream_id = match a {
        BelValue::Nil => StreamTable::stdin_id(),
        BelValue::Stream(id) => id,
        _ => return Err(BelError::TypeError("rdb: arg must be a stream or nil".into())),
    };

    match streams.read_bit(stream_id) {
        Ok(Some(true)) => Ok(BelValue::Char(CharId(b'1'))),
        Ok(Some(false)) => Ok(BelValue::Char(CharId(b'0'))),
        Ok(None) => Ok(BelValue::Nil),
        Err(ref e) if e.to_string().contains("eof") => Ok(BelValue::Symbol(sym::EOF)),
        Err(e) => Err(e),
    }
}

/// (ops name dir) — open a stream.
/// name is a char list (file path), dir is symbol `in` or `out`.
fn prim_ops(
    a: BelValue,
    b: BelValue,
    heap: &Heap,
    symbols: &SymbolTable,
    streams: &mut StreamTable,
) -> BelResult<BelValue> {
    // Extract file name from char list
    let mut name = String::new();
    let mut current = a;
    loop {
        match current {
            BelValue::Nil => break,
            BelValue::Pair(id) => {
                let ch = heap.car(id);
                match ch {
                    BelValue::Char(cid) => name.push(CharTable::to_char(cid)),
                    _ => return Err(BelError::TypeError("ops: name list element is not a char".into())),
                }
                current = heap.cdr(id);
            }
            _ => return Err(BelError::TypeError("ops: name is not a proper list".into())),
        }
    }

    // Extract direction
    let dir = match b {
        BelValue::Symbol(id) => symbols.name(id).to_string(),
        BelValue::Nil => "in".to_string(), // default to input
        _ => return Err(BelError::TypeError("ops: direction must be a symbol".into())),
    };

    let stream_id = streams.open(&name, &dir)?;
    Ok(BelValue::Stream(stream_id))
}

/// (cls stream) — close a stream.
fn prim_cls(a: BelValue, streams: &mut StreamTable) -> BelResult<BelValue> {
    match a {
        BelValue::Stream(id) => {
            streams.close(id)?;
            Ok(BelValue::Nil)
        }
        BelValue::Nil => Ok(BelValue::Nil), // closing nil is a no-op
        _ => Err(BelError::TypeError("cls: not a stream".into())),
    }
}

/// (stat stream) — return the status of a stream: closed, in, or out.
fn prim_stat(
    a: BelValue,
    symbols: &mut SymbolTable,
    streams: &StreamTable,
) -> BelResult<BelValue> {
    match a {
        BelValue::Stream(id) => {
            let status = streams.status(id);
            let sym_id = symbols.intern(status);
            Ok(BelValue::Symbol(sym_id))
        }
        BelValue::Nil => {
            // nil represents stdin/stdout — it's open for both, but call it "in"
            Ok(BelValue::Symbol(sym::IN))
        }
        _ => Err(BelError::TypeError("stat: not a stream".into())),
    }
}

/// (coin) — return t or nil randomly with equal probability.
fn prim_coin() -> BelResult<BelValue> {
    let mut rng = rand::thread_rng();
    if rng.gen::<bool>() {
        Ok(BelValue::Symbol(sym::T))
    } else {
        Ok(BelValue::Nil)
    }
}

/// (sys cmd) — execute a shell command. Returns nil.
fn prim_sys(a: BelValue, heap: &Heap) -> BelResult<BelValue> {
    // Extract command string from char list
    let mut cmd = String::new();
    let mut current = a;
    loop {
        match current {
            BelValue::Nil => break,
            BelValue::Pair(id) => {
                let ch = heap.car(id);
                match ch {
                    BelValue::Char(cid) => cmd.push(CharTable::to_char(cid)),
                    _ => return Err(BelError::TypeError("sys: command list element is not a char".into())),
                }
                current = heap.cdr(id);
            }
            _ => return Err(BelError::TypeError("sys: command is not a proper list".into())),
        }
    }

    if !cmd.is_empty() {
        // Execute the command
        let _ = std::process::Command::new("cmd")
            .args(["/C", &cmd])
            .status();
    }

    Ok(BelValue::Nil)
}
