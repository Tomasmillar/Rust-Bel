use crate::chars::CharTable;
use crate::heap::Heap;
use crate::symbol::{sym, SymbolTable};
use crate::value::BelValue;

/// Print a Bel value to a string.
pub fn print_val(val: BelValue, heap: &Heap, symbols: &SymbolTable) -> String {
    let mut out = String::new();
    print_inner(val, heap, symbols, &mut out, 0);
    out
}

fn print_inner(val: BelValue, heap: &Heap, symbols: &SymbolTable, out: &mut String, depth: usize) {
    if depth > 1000 {
        out.push_str("...");
        return;
    }

    match val {
        BelValue::Nil => out.push_str("nil"),
        BelValue::Symbol(id) => {
            let name = symbols.name(id);
            // Check if name needs escaping
            if name.is_empty() || name.contains(' ') || name.contains('(') || name.contains(')')
                || name.contains('[') || name.contains(']') || name.contains('"')
                || name.contains(';') || name.contains('\\')
            {
                out.push('|');
                out.push_str(name);
                out.push('|');
            } else {
                out.push_str(name);
            }
        }
        BelValue::Char(id) => {
            out.push('\\');
            if let Some(name) = CharTable::char_name(id) {
                out.push_str(name);
            } else if CharTable::is_printable(id) {
                out.push(CharTable::to_char(id));
            } else {
                // Print as numeric escape
                out.push_str(&format!("x{:02x}", id.0));
            }
        }
        BelValue::Stream(id) => {
            out.push_str(&format!("<stream {}>", id.0));
        }
        BelValue::Pair(id) => {
            // Check for special patterns first

            // (quote x) -> 'x
            let car = heap.car(id);
            let cdr = heap.cdr(id);
            if car == BelValue::Symbol(sym::QUOTE) {
                if let BelValue::Pair(cdr_id) = cdr {
                    let cadr = heap.car(cdr_id);
                    let cddr = heap.cdr(cdr_id);
                    if cddr.is_nil() {
                        out.push('\'');
                        print_inner(cadr, heap, symbols, out, depth + 1);
                        return;
                    }
                }
            }

            // Check for string: proper list of all chars
            if is_string(val, heap) {
                print_string(val, heap, out);
                return;
            }

            // Check for number: (lit num real imag)
            if let Some(num_str) = try_print_number(val, heap) {
                out.push_str(&num_str);
                return;
            }

            // Regular list/pair
            out.push('(');
            print_inner(car, heap, symbols, out, depth + 1);

            let mut current = cdr;
            loop {
                match current {
                    BelValue::Nil => break,
                    BelValue::Pair(_) => {
                        // Check if the tail is a special printable form (number, string, quote)
                        if try_print_number(current, heap).is_some()
                            || is_string(current, heap)
                        {
                            out.push_str(" . ");
                            print_inner(current, heap, symbols, out, depth + 1);
                            break;
                        }
                        let pid = current.as_pair().unwrap();
                        out.push(' ');
                        let pcar = heap.car(pid);
                        let pcdr = heap.cdr(pid);
                        print_inner(pcar, heap, symbols, out, depth + 1);
                        current = pcdr;
                    }
                    _ => {
                        out.push_str(" . ");
                        print_inner(current, heap, symbols, out, depth + 1);
                        break;
                    }
                }
            }
            out.push(')');
        }
    }
}

/// Check if a value is a proper list of all characters (a string).
fn is_string(val: BelValue, heap: &Heap) -> bool {
    let mut current = val;
    let mut has_chars = false;
    loop {
        match current {
            BelValue::Nil => return has_chars,
            BelValue::Pair(id) => {
                let car = heap.car(id);
                if !car.is_char() {
                    return false;
                }
                has_chars = true;
                current = heap.cdr(id);
            }
            _ => return false,
        }
    }
}

/// Print a character list as a string.
fn print_string(val: BelValue, heap: &Heap, out: &mut String) {
    out.push('"');
    let mut current = val;
    while let BelValue::Pair(id) = current {
        let car = heap.car(id);
        if let BelValue::Char(cid) = car {
            let ch = cid.0;
            match ch {
                b'"' => out.push_str("\\\""),
                b'\\' => out.push_str("\\\\"),
                b'\n' => out.push_str("\\n"),
                b'\t' => out.push_str("\\t"),
                b'\r' => out.push_str("\\r"),
                c if c >= 32 && c < 127 => out.push(c as char),
                c => out.push_str(&format!("\\x{:02x}", c)),
            }
        }
        current = heap.cdr(id);
    }
    out.push('"');
}

/// Try to print a number: (lit num (sign numer denom) (sign numer denom))
fn try_print_number(val: BelValue, heap: &Heap) -> Option<String> {
    // Check for (lit num real imag)
    let id = val.as_pair()?;
    let car = heap.car(id);
    if car != BelValue::Symbol(sym::LIT) {
        return None;
    }
    let cdr = heap.cdr(id);
    let cdr_id = cdr.as_pair()?;
    let tag = heap.car(cdr_id);
    if tag != BelValue::Symbol(sym::NUM) {
        return None;
    }
    let cddr = heap.cdr(cdr_id);
    let cddr_id = cddr.as_pair()?;
    let real_part = heap.car(cddr_id);
    let cdddr = heap.cdr(cddr_id);
    let cdddr_id = cdddr.as_pair()?;
    let _imag_part = heap.car(cdddr_id);

    // Decode real part: (sign numer denom)
    let real_id = real_part.as_pair()?;
    let sign_val = heap.car(real_id);
    let rest = heap.cdr(real_id);
    let rest_id = rest.as_pair()?;
    let numer_val = heap.car(rest_id);
    let rest2 = heap.cdr(rest_id);
    let rest2_id = rest2.as_pair()?;
    let denom_val = heap.car(rest2_id);

    let sign = if sign_val == BelValue::Symbol(sym::MINUS) { -1i64 } else { 1 };
    let numer = unary_len(numer_val, heap);
    let denom = unary_len(denom_val, heap);

    if denom == 0 {
        return Some("NaN".into());
    }

    // Check for integer
    if denom == 1 {
        return Some(format!("{}", sign * numer as i64));
    }

    // Check if it simplifies to a nice decimal
    let g = gcd(numer, denom);
    let n = numer / g;
    let d = denom / g;

    if d == 1 {
        return Some(format!("{}", sign * n as i64));
    }

    // Print as fraction
    if sign < 0 {
        Some(format!("-{}/{}", n, d))
    } else {
        Some(format!("{}/{}", n, d))
    }
}

/// Count the length of a unary number (list of t).
fn unary_len(val: BelValue, heap: &Heap) -> u64 {
    let mut count = 0;
    let mut current = val;
    while let BelValue::Pair(id) = current {
        count += 1;
        current = heap.cdr(id);
    }
    count
}

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd(b, a % b) }
}
