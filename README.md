# Bel

A faithful interpreter for Paul Graham's [Bel](https://paulgraham.com/bel.html) programming language, written in Rust.

Bel is a Lisp that defines itself. Starting from 16 primitives and 7 special forms, the source file `bel.bel` builds an entire language: numbers, arithmetic, macros, error handling, I/O, threads, and a metacircular evaluator. But `bel.bel` can't run on its own — it needs a foundation to execute on.

This project is that foundation. It provides the minimum Rust substrate needed to load and run the unmodified `bel.bel`, letting Graham's definitions build the language from the ground up. Nothing is reimplemented — `map`, `append`, `+`, `sort`, and everything else come from `bel.bel` itself.

## Building

Requires Rust (2021 edition). The only dependency is `rand`.

### macOS / Linux

If you don't have Rust installed:

```sh
chmod +x scripts/install_rust.sh
./scripts/install_rust.sh
```

Then build and run:

```sh
cargo build --release
./target/release/bel --load bel.bel
```

### Windows

If you don't have Rust installed, open PowerShell and run:

```powershell
.\scripts\install_rust.ps1
```

Then build and run:

```powershell
cargo build --release
.\target\release\bel.exe --load bel.bel
```

## Usage

### Load bel.bel and start a REPL

```
bel --load bel.bel
```

This loads the complete language definition, then drops into an interactive session:

```
Loading bel.bel... done (904 expressions)
Bel interpreter (loaded: bel.bel)
  Heap: 2841906 cells used, Symbols: 438 interned
Ready.

> (+ 2 3)
5
> (map car '((a b) (c d) (e f)))
(a c e)
> (sort < '(5 1 3 2 4))
(1 2 3 4 5)
```

### Native arithmetic mode

```
bel --native-math --load bel.bel
```

By default, all arithmetic goes through `bel.bel`'s number tower — list-based unary numbers, exactly as Graham defined them. This is faithful to the spec but slow. The `--native-math` flag intercepts calls to `+`, `-`, `*`, `/`, and `<` and performs them with native Rust arithmetic for much better performance.

```
Bel interpreter (loaded: bel.bel)
  Heap: 2841906 cells used, Symbols: 438 interned
  Mode: native arithmetic enabled
Ready.
```

### Bare REPL (primitives only)

```
bel
```

Without `--load`, only the 16 primitives and 7 special forms are available. Useful for debugging the substrate itself.

```
> (join 'a 'b)
(a . b)
> (car '(x y z))
x
> (id 'a 'a)
t
```

### Piped input

```
echo (join 'a 'b) | bel --load bel.bel
```

### Running the test suite

The test suite consists of ~150 examples drawn from Graham's language guide:

```
bel --load bel.bel < tests/test_guide_simple.bel
```

### Debug tracing

Set `BEL_TRACE=1` to see evaluation steps (very verbose):

```
BEL_TRACE=1 bel --load bel.bel
```

## What's provided in Rust

Everything below is the Rust substrate. Everything else comes from `bel.bel`.

### Four data types

Bel has exactly four types. All values are one of these:

- **Pairs** — mutable cons cells (`join`, `car`, `cdr`, `xar`, `xdr`). Heap-allocated with mark-sweep garbage collection.
- **Symbols** — interned names. `(id 'foo 'foo)` is `t` because both resolve to the same identity.
- **Characters** — ASCII 0-127, each with a binary representation as specified by the `chars` global.
- **Streams** — open I/O handles for bit-level reading and writing.

`nil` is both a symbol and the empty list. There are no native numbers, strings, or booleans — numbers are nested pairs, strings are lists of characters, and truth is anything that isn't `nil`.

### 16 primitives

These are the axioms Bel assumes exist. They cannot be defined in Bel itself:

| Primitive | Purpose |
|-----------|---------|
| `id`      | Identity (pointer equality) |
| `join`    | Create a new pair |
| `car`     | First element of a pair |
| `cdr`     | Rest of a pair |
| `type`    | Returns `symbol`, `pair`, `char`, or `stream` |
| `xar`     | Mutate the car of a pair |
| `xdr`     | Mutate the cdr of a pair |
| `sym`     | Character list to symbol |
| `nom`     | Symbol to character list |
| `wrb`     | Write a bit to a stream |
| `rdb`     | Read a bit from a stream |
| `ops`     | Open a stream |
| `cls`     | Close a stream |
| `stat`    | Stream status |
| `coin`    | Random `t` or `nil` |
| `sys`     | Execute a shell command |

### 7 special forms

These control evaluation and cannot be expressed as functions:

| Form | Purpose |
|------|---------|
| `quote` | Return argument unevaluated |
| `if` | Conditional evaluation |
| `where` | Settable location context (makes `set` work on place expressions) |
| `dyn` | Dynamic variable binding |
| `after` | Guaranteed cleanup (like `finally`) |
| `ccc` | Call with current continuation |
| `thread` | Spawn a green thread |

### The evaluation engine

The evaluator is a direct translation of `bel.bel`'s own `ev`/`mev`/`sched` state machine into Rust. State is an `(s r m)` triple:

- **s** — expression stack: what to evaluate next
- **r** — return stack: computed values
- **m** — metadata: other threads and the global environment

The machine loops iteratively through `mev`, never recursing into the host language's call stack. This is what makes continuations and threads possible without host-language tricks.

### A bootstrap reader and printer

Bel defines its own reader and printer in `bel.bel`, but something has to read `bel.bel` before those exist. The Rust reader handles full Bel syntax:

- Lists and dot notation: `(a b c)`, `(a . b)`
- Quote: `'x`
- Backquote with unquote and splicing: `` `(a ,b ,@c) ``
- Bracket functions: `[f _ x]` expands to `(fn (_) (f _ x))`
- Characters: `\a`, `\bel`, `\sp`, `\lf`, `\xHH`
- Strings: `"hello"` (list of characters)
- Shared structure: `#1=(a b) #1`
- Intrasymbol notation: `f:g` (compose), `~f` (negate), `a.b` (call), `a!b` (quote-call)
- Numbers: `42`, `2/3`, `1.5`
- Comments: `; to end of line`

### Garbage collection

Mark-sweep with adaptive thresholds. GC roots are traced from all machine state (global environment, stacks, forms, streams). Freed cells are poisoned with a marker value to detect use-after-free bugs.

## Design decisions

Bel's specification deliberately leaves several choices to the implementer. These are the ones this project made, and why.

### The evaluator is a translated state machine, not an "inspired by" interpreter

Most Bel implementations write a conventional tree-walking interpreter and handle Bel's special cases (continuations, threads, dynamic binding) with host-language mechanisms. This project translates `bel.bel`'s evaluation model directly: explicit expression and return stacks, `fut` entries for multi-step operations, `smark`-tagged metadata for bindings and protected expressions. The Rust code mirrors the Bel code.

This matters because `ccc` captures the stacks as data. If the stacks don't exist as data — if evaluation state is implicit in host-language recursion — continuations require workarounds (CPS transforms, `setjmp`/`longjmp`, coroutines). The direct translation makes `ccc` and `thread` fall out naturally.

### Everything loads from bel.bel, unmodified

Other implementations fork `bel.bel`, pre-compile it, or reimplement its definitions natively in the host language. This project loads Graham's file as-is. The standard library, number tower, macro system, reader, and printer are all defined by `bel.bel` at runtime.

The cost is performance. The benefit is that the running system matches the specification exactly, and any future optimization can be verified against the unmodified `bel.bel` as ground truth.

### No native numbers by default

Bel defines numbers as nested pairs of characters. `3` is `(lit num ((\0 \0 \0 \0 \0 \1 \1) . nil))` — a sign-magnitude binary representation built from character lists. Arithmetic is recursive list manipulation.

Every other Bel implementation that got far enough to care about performance added native host-language numbers as a shortcut. By default, this project does not — `(+ 1 2)` walks the full arithmetic chain defined in `bel.bel`. This is slow, but it is what the spec defines. The `--native-math` flag enables native Rust arithmetic as an opt-in optimization.

### Characters are ASCII 0-127

The spec says `chars` should include "at least those in the Bel source." This project uses 7-bit ASCII (128 characters), each with its standard binary representation. This covers everything in `bel.bel` and is straightforward to extend.

### Streams connect to files

`ops` opens a stream for a named "place." This project interprets place names as file paths: `(ops "foo.txt" 'out)` opens a file. The default streams (`ins`/`outs`) connect to stdin/stdout.

Bit-level I/O is implemented as specified — `wrb` and `rdb` operate on individual bits (MSB-first), with byte buffering for efficiency.

### Errors return to the REPL

The spec does not define what `err` does. When an error is signalled and no dynamic binding for `err` exists, this project prints the error and returns control to the REPL. Error values are symbols like `unbound`, `overargs`, or `mistype`.

### `sys` executes shell commands

`(sys "dir")` runs a command through the system shell. On Windows this uses `cmd /C`; on Unix it would use `/bin/sh -c`.

### `coin` uses a PRNG

`(coin)` returns `t` or `nil` with equal probability using the system's pseudorandom number generator.

### Values are 8 bytes with Copy semantics

`BelValue` is an enum that fits in 8 bytes: a discriminant tag plus a 32-bit payload (pair index, symbol ID, character ID, or stream ID). Values are `Copy` — passed by value with no allocation. All pair data lives on the heap, referenced by index.

This is a significant advantage over managed-language implementations where every value carries object headers and indirection. It also means GC only needs to trace the heap, not the Rust stack.

### GC is stop-the-world mark-sweep

The simplest correct approach. The threshold adapts based on heap occupancy to avoid thrashing. The heap has a hard capacity limit (default 100M cells) to prevent runaway allocation.

A more sophisticated collector (generational, incremental) could be added as a stage 2 optimization. Mark-sweep is correct and predictable, which is what stage 1 needs.

### Fuel-based execution limits

The machine has a step counter (default 1 billion steps). If an expression takes more steps than this, evaluation halts with a `FuelExhausted` error. This prevents infinite loops from hanging the interpreter and makes it safe to experiment.

## Project structure

```
src/
  main.rs        Entry point, REPL, file loading
  lib.rs         Module exports
  value.rs       BelValue enum (8-byte tagged union)
  heap.rs        Cons cell heap with mark-sweep GC
  symbol.rs      Symbol table with interning
  chars.rs       ASCII character table with binary representations
  stream.rs      Bit-level I/O streams
  primitives.rs  The 16 Bel primitives
  globals.rs     Initial global environment
  eval.rs        The evaluator (ev/mev/sched state machine)
  reader.rs      Bootstrap reader for Bel syntax
  printer.rs     Value printer
  error.rs       Error types

bel.bel          Paul Graham's complete language definition
docs/
  bel_guide.txt          Graham's language guide
  Bel_Project_Brief.md   Project design document
scripts/
  install_rust.sh        Rust installer for macOS/Linux
  install_rust.ps1       Rust installer for Windows
tests/
  test_guide_simple.bel  ~150 test cases from the guide
  run_tests.sh           Test runner (bash)
  run_tests.ps1          Test runner (PowerShell)
```

## Context

Bel was released by Paul Graham in October 2019 as a formal specification — a language defined in itself, not a runnable system. Several implementations exist in Perl, Haskell, Clojure, C, and other languages, but most reimplement `bel.bel`'s definitions natively in the host language rather than loading the file directly.

This is one of the few implementations that loads the unmodified `bel.bel` and provides all 7 special forms, including `ccc` (continuations) and `thread` (green threads) — two features that many implementations omit despite being part of the axiomatic foundation.

The goal is fidelity, not speed. A future stage 2 could explore optimizations discovered by observing the running spec, verified against `bel.bel` as the permanent authority.

## License

The `bel.bel` and `docs/bel_guide.txt` files are Graham's work.
