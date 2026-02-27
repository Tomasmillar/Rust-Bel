use std::io::{self, BufRead, IsTerminal, Read, Write};
use std::time::Instant;

use bel::eval::Machine;
use bel::heap::Heap;
use bel::printer;
use bel::reader;
use bel::symbol::SymbolTable;
use bel::value::BelValue;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut machine = Machine::new(
        100_000_000, // 100M cons cell capacity
        u64::MAX, // no step limit — use Ctrl+C to interrupt
    )
    .expect("Failed to initialize machine");

    // Install bootstrap forms
    machine
        .install_bootstrap_forms()
        .expect("Failed to install bootstrap forms");

    // Enable tracing for debugging (set via env var BEL_TRACE=1)
    if std::env::var("BEL_TRACE")
        .map(|v| v == "1")
        .unwrap_or(false)
    {
        machine.trace = true;
    }

    // Process command-line flags
    let mut load_files: Vec<String> = Vec::new();
    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "--load" => {
                if i + 1 < args.len() {
                    load_files.push(args[i + 1].clone());
                    i += 2;
                } else {
                    eprintln!("--load requires a file path");
                    std::process::exit(1);
                }
            }
            "--native-math" => {
                machine.native_arithmetic = true;
                i += 1;
            }
            "--help" | "-h" => {
                println!("Usage: bel [OPTIONS]");
                println!();
                println!("Options:");
                println!("  --load <file>    Load a Bel source file before starting the REPL");
                println!("  --native-math    Use native Rust arithmetic instead of bel.bel's number tower");
                println!("  --help, -h       Show this help message");
                println!();
                println!("Environment variables:");
                println!("  BEL_TRACE=1    Enable evaluation tracing");
                std::process::exit(0);
            }
            other => {
                eprintln!("Unknown argument: {}", other);
                eprintln!("Try 'bel --help' for usage information.");
                std::process::exit(1);
            }
        }
    }

    // Load files silently
    for path in &load_files {
        load_file(&mut machine, path);
    }

    let stdin = io::stdin();

    if stdin.is_terminal() {
        if load_files.is_empty() {
            println!("Bel interpreter");
        } else {
            println!("Bel interpreter (loaded: {})", load_files.join(", "));
        }
        println!(
            "  Heap: {} cells used, Symbols: {} interned",
            machine.heap.total_cells(),
            machine.symbols.count()
        );
        if machine.native_arithmetic {
            println!("  Mode: native arithmetic enabled");
        }
        println!("Ready.\n");
        run_interactive(&mut machine);
    } else {
        run_piped(&mut machine);
    }
}

/// Try to extract definition info from a parsed expression.
/// Returns (form, name) like ("def", "no") or ("mac", "fn") or ("set", "vmark").
fn extract_def_name(expr: BelValue, heap: &Heap, symbols: &SymbolTable) -> Option<(String, String)> {
    let pair_id = expr.as_pair()?;
    let form_sym = heap.car(pair_id).as_symbol()?;
    let form_name = symbols.name(form_sym).to_string();

    if form_name != "def" && form_name != "mac" && form_name != "set" {
        return None;
    }

    let cdr = heap.cdr(pair_id);
    let cdr_pair = cdr.as_pair()?;
    let name_sym = heap.car(cdr_pair).as_symbol()?;
    let def_name = symbols.name(name_sym).to_string();

    Some((form_name, def_name))
}

/// Load a Bel source file with progress reporting.
///
/// The bootstrap process:
///   Rust provides 16 primitives (id, join, car, cdr, ...) and 7 axiomatic
///   special forms (quote, if, where, dyn, after, ccc, thread) that are
///   always native. It also provides 6 temporary bootstrap forms (set, def,
///   fn, mac, do, let) — just enough to start evaluating bel.bel.
///
///   As bel.bel loads, it redefines fn/do/let/def/mac early on, then builds
///   the full evaluator, resets the forms table (replacing the native special
///   forms with Bel closures), redefines set last, and finally loads the
///   reader/printer. After loading, everything runs through bel.bel — only
///   the 16 primitives remain as native Rust.
fn load_file(machine: &mut Machine, path: &str) {
    let input = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error loading {}: {}", path, e);
            std::process::exit(1);
        }
    };

    let start = Instant::now();
    eprintln!("Loading {}", path);
    eprintln!(
        "  Bootstrap: 16 primitives + 7 native forms + 6 temporary forms (set, def, fn, mac, do, let)"
    );
    eprintln!("  [1/9] Base utilities (bootstrap: evaluating with native set, def, fn)...");

    let mut pos = 0;
    let mut count = 0;
    let mut phase = 1u32;
    loop {
        let result = reader::read_one_at(&input, pos, &mut machine.heap, &mut machine.symbols);
        match result {
            Ok(Some((expr, new_pos))) => {
                pos = new_pos;
                count += 1;

                // Detect phase transitions by inspecting the definition name
                if let Some((form, name)) = extract_def_name(expr, &machine.heap, &machine.symbols) {
                    if phase < 2 && form == "mac" && name == "fn" {
                        phase = 2;
                        eprintln!(
                            "  [2/9] Replacing bootstrap fn, do, let, def, mac with bel.bel macros..."
                        );
                    } else if phase < 3 && form == "def" && name == "=" {
                        phase = 3;
                        eprintln!(
                            "  [3/9] Core language (bootstrap: still using native set, quote, if)..."
                        );
                    } else if phase < 4 && form == "def" && name == "bel" {
                        phase = 4;
                        eprintln!(
                            "  [4/9] Metacircular evaluator (bel, mev, ev, evcall, applylit)..."
                        );
                    } else if phase < 5 && form == "set" && name == "forms" {
                        phase = 5;
                        eprintln!(
                            "  [5/9] Resetting forms table -- native quote/if/where/dyn/after/ccc/thread replaced by bel.bel closures..."
                        );
                    } else if phase < 6 && form == "def" && name == "con" {
                        phase = 6;
                        eprintln!("  [6/9] Higher-order functions (compose, combine, fold)...");
                    } else if phase < 7 && form == "set" && name == "i0" {
                        phase = 7;
                        eprintln!("  [7/9] Number tower (integer, rational, complex arithmetic)...");
                    } else if phase < 8 && form == "mac" && name == "set" {
                        phase = 8;
                        eprintln!(
                            "  [8/9] Replacing bootstrap set -- last native bootstrap form removed..."
                        );
                    } else if phase < 9 && form == "def" && name == "read" {
                        phase = 9;
                        eprintln!("  [9/9] Reader & printer...");
                    }
                }

                match machine.eval(expr) {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("\nError at expression {}: {}", count, e);
                        std::process::exit(1);
                    }
                }
            }
            Ok(None) => break,
            Err(e) => {
                eprintln!("\nRead error at position {}: {}", pos, e);
                std::process::exit(1);
            }
        }
    }

    let elapsed = start.elapsed();
    eprintln!(
        "  Loaded {} expressions in {:.2}s ({} symbols interned)",
        count,
        elapsed.as_secs_f64(),
        machine.symbols.count()
    );
    eprintln!(
        "  All bootstrap forms replaced. Running on bel.bel (16 primitives remain native)."
    );
}

/// Interactive REPL: accumulate lines until parens are balanced.
fn run_interactive(machine: &mut Machine) {
    let stdin = io::stdin();
    let mut buf = String::new();
    let mut depth: i32 = 0;

    loop {
        if depth == 0 {
            print!("> ");
        } else {
            print!("  ");
        }
        io::stdout().flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => break, // EOF
            Ok(_) => {}
            Err(e) => {
                eprintln!("Read error: {}", e);
                break;
            }
        }

        // Track paren depth (naive but sufficient for well-formed input)
        for ch in line.chars() {
            match ch {
                '(' | '[' => depth += 1,
                ')' | ']' => depth -= 1,
                _ => {}
            }
        }

        buf.push_str(&line);

        if depth <= 0 {
            depth = 0;
            let input = buf.trim().to_string();
            buf.clear();

            if input.is_empty() {
                continue;
            }

            eval_and_print(&input, machine);
        }
    }
}

/// Piped mode: read all input, then parse and evaluate one expression at a time.
/// This avoids a GC bug where pre-parsed ASTs on the Rust stack get swept.
fn run_piped(machine: &mut Machine) {
    let mut input = String::new();
    io::stdin()
        .read_to_string(&mut input)
        .expect("Failed to read input");

    let input = input.trim().to_string();
    if input.is_empty() {
        return;
    }

    // Parse and evaluate one expression at a time so that unparsed text
    // doesn't create heap references that the GC can't see.
    let mut pos = 0;
    loop {
        let result = reader::read_one_at(&input, pos, &mut machine.heap, &mut machine.symbols);
        match result {
            Ok(Some((expr, new_pos))) => {
                pos = new_pos;
                match machine.eval(expr) {
                    Ok(val) => {
                        let output = printer::print_val(val, &machine.heap, &machine.symbols);
                        println!("{}", output);
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
                    }
                }
            }
            Ok(None) => break,
            Err(e) => {
                eprintln!("Read error: {}", e);
                break;
            }
        }
    }
}

/// Evaluate one or more expressions in a string and print results.
fn eval_and_print(input: &str, machine: &mut Machine) {
    let mut pos = 0;
    loop {
        match reader::read_one_at(input, pos, &mut machine.heap, &mut machine.symbols) {
            Ok(Some((expr, new_pos))) => {
                pos = new_pos;
                match machine.eval(expr) {
                    Ok(val) => {
                        let output = printer::print_val(val, &machine.heap, &machine.symbols);
                        println!("{}", output);
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
                    }
                }
            }
            Ok(None) => break,
            Err(e) => {
                eprintln!("{}", e);
                break;
            }
        }
    }
}
