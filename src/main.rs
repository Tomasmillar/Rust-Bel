use std::io::{self, BufRead, IsTerminal, Read, Write};

use bel::eval::Machine;
use bel::printer;
use bel::reader;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let mut machine = Machine::new(
        100_000_000, // 100M cons cell capacity
        1_000_000_000, // 1 billion step limit
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

/// Load a file silently (used for --load bel.bel).
fn load_file(machine: &mut Machine, path: &str) {
    let input = match std::fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error loading {}: {}", path, e);
            std::process::exit(1);
        }
    };

    eprint!("Loading {}...", path);

    let mut pos = 0;
    let mut count = 0;
    loop {
        let result = reader::read_one_at(&input, pos, &mut machine.heap, &mut machine.symbols);
        match result {
            Ok(Some((expr, new_pos))) => {
                pos = new_pos;
                count += 1;
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

    eprintln!(" done ({} expressions)", count);
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
