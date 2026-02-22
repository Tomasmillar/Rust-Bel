# Project: Giving Bel Its First Turtle

## What This Is

Paul Graham's Bel is a programming language that defines itself. Starting from 16 primitives and 7 special forms, the source file bel.bel builds an interpreter, a reader, a printer, a number system, I/O, macros, error handling, threads — the complete language. Everything defined as code in the language itself.

But it can't run. The interpreter is written in Bel, but there's nothing to execute it. It's turtles all the way down, and someone has to provide the first one.

This project is that first turtle: the minimum foundation needed to make bel.bel execute. We are not trying to make Bel fast. We are trying to make Bel *exist* — as faithfully as possible to Graham's spec.

Everything runs through bel.bel. We don't reimplement the language — we provide the ground floor and let Graham's building stand up on it. If `(+ 1 2)` takes ten thousand steps through the unary arithmetic chain, that's what it takes. That IS Bel. That's what he defined.

The interesting output is twofold: a working Bel, and the decisions the spec forced us to make — the ones Graham deliberately left open for implementers to discover.

A future stage 2 project may explore optimizations — abstractions discovered by observing the running spec, verified against it, with bel.bel as the permanent fallback and authority. But that grows out of what we learn here. This is stage 1: make it exist.

## Source Documents

- **bel_guide.txt** — Graham's explanation of the language (in this directory). Read this first — it explains the why behind everything.
- **bel.bel** — The complete language defined in itself (~900 lines, at the project root). This is both the specification and the program we're trying to run.

## What We Must Provide

Bel defines everything in terms of everything else, except for these. We implement these in Rust. Everything else comes from bel.bel.

### 1. Data Representation

Bel has four types: symbols, pairs, characters, streams. It says what they are, not how to store them. We need:
- Pairs (cons cells) — heap-allocated, mutable (via xar/xdr), garbage-collected.
- Symbols — interned so that `(id 'foo 'foo)` is true. A symbol table mapping names to unique references.
- Characters — each with a binary representation (a string of \1 and \0), as specified in the `chars` global.
- Streams — wrapping real I/O handles.
- Nil — both a symbol and the empty list. Also represents falsity.

Everything in Bel is built from these. Numbers are nested pairs. Functions are lists. There are no special optimized representations — if the spec says it's a list, it's a list.

### 2. The 16 Primitives

The axioms. Bel assumes these exist and never defines them:

| Primitive | Purpose |
|-----------|---------|
| `id`      | Identical? (pointer equality) |
| `join`    | Make a new pair |
| `car`     | First half of pair (nil→nil, other atoms→error) |
| `cdr`     | Second half of pair (nil→nil, other atoms→error) |
| `type`    | Returns `symbol`, `pair`, `char`, or `stream` |
| `xar`     | Mutate the car of a pair |
| `xdr`     | Mutate the cdr of a pair |
| `sym`     | String (list of chars) → symbol |
| `nom`     | Symbol → fresh list of name's characters |
| `wrb`     | Write a bit (\1 or \0) to a stream |
| `rdb`     | Read a bit from a stream (\1, \0, nil if unavailable, eof if done) |
| `ops`     | Open a stream (by name string, for `in` or `out`) |
| `cls`     | Close a stream |
| `stat`    | Stream status: `closed`, `in`, or `out` |
| `coin`    | Random `t` or `nil` |
| `sys`     | Send command string to operating system |

Missing arguments default to nil. Extra arguments cause an error.

### 3. The Evaluation Loop

Bel's evaluator is defined in bel.bel as `ev`, `mev`, and `sched`. We translate this machine into Rust — not "write an interpreter inspired by Bel," but translate the actual machine. The spec IS the implementation.

The machine:
- State is `(s r m)` — expression stack, return stack, and `(other-threads, globals)`.
- Each entry on `s` is `(expression, lexical-environment)`, or a metadata entry marked with `smark`.
- **Returning a value** = pushing onto `r` and calling `mev`.
- **Evaluating** = popping from `s` and dispatching: literal? variable? special form? function call?
- The stack carries metadata entries for dynamic bindings (`bind`), settable locations (`loc`), continuation closures (`fut`), and protected expressions (`prot`).
- Thread scheduling happens in `mev` — `lock` prevents switching, otherwise threads rotate.

The evaluator never returns in the host language sense. It loops through mev/sched until the expression stack is empty.

### 4. A Reader (For Bootstrapping)

Bel defines a reader in bel.bel (rdex and friends), but we need to read bel.bel before we can run it. So we implement a reader in Rust. This is bootstrapping infrastructure — once bel.bel loads, the Bel-defined reader exists too.

Full Bel read syntax:
- Lists with dot notation: `(a . b)`, `(a b c)`
- Quote: `'x` → `(quote x)`
- Backquote/comma: `` `(a ,b ,@c) ``
- Square brackets: `[f _ x]` → `(fn (_) (f _ x))`
- Characters: `\a`, `\bel`, `\sp`, `\lf`
- Strings: `"hello"` (proper lists of characters)
- Shared structure: `#1=(a b) #1`
- Intrasymbol characters: `a:b` → `(compose a b)`, `~f` → `(compose no f)`, `a.b` → `(a b)`, `a!b` → `(a 'b)`, `x|f` → `(t x f)`
- Numbers: `42`, `2/3`, `1.5`, `4-1/2i`
- Comments: `;` to end of line
- Symbol escaping: `¦foo bar¦`

### 5. A Printer (For Bootstrapping)

Same bootstrapping situation. Implement in Rust so we can display results. Must handle shared structure (`#n=`), strings, characters, numbers in conventional notation, symbol escaping with `¦`. Once bel.bel loads, the Bel-defined printer is available too.

### 6. Garbage Collection

Pairs are heap-allocated and Bel creates many of them. We need a garbage collector. Start with the simplest correct approach — stop-the-world mark-sweep.

## Decisions The Spec Forces Us To Make

Graham deliberately left these open. We must choose, and our choices are part of the project's output.

### Which characters exist?

`chars` is "a list of all characters" with binary representations. Bel doesn't say which characters exist or what their binary representations are. We're choosing a character encoding. Graham says chars "should include at least those in the Bel source."

Starting point: ASCII (128 characters) with standard 7-bit binary representations. This covers everything in bel.bel. Expand later if needed.

### What do streams connect to?

`ops` opens a stream for a named place. But what places exist? `ins` and `outs` default to nil, which represents "the initial input and output streams" — presumably the terminal.

Starting point: file paths. `(ops "foo.txt" 'out)` opens a file for writing. `nil` streams are the terminal (stdin/stdout).

### What do errors look like?

Bel "doesn't specify a global binding for err; this is something for a programming environment built on top of Bel to do." When something goes wrong, `err` is called with a value describing the error. But what that value is, and what happens when `err` is called without a dynamic binding, is up to us.

Starting point: print the error value and return to the REPL. Error values are whatever sigerr passes — usually symbols like `'unbound` or `'overargs`.

### What does `sys` do?

"Sends x as a command to the operating system." One sentence.

Starting point: shell execution via the system's default shell. Return nil.

### How random is `coin`?

"Returns either t or nil randomly."

Starting point: pseudorandom, using the system's default RNG.

### What is the REPL?

No REPL is defined in the spec. The guide shows `>` prompts informally.

Starting point: print `> `, read an expression, evaluate it, print the result. On error, print the error and continue.

## What's Technically Difficult

### The Evaluation Model

Bel's evaluator is a state machine communicating through stacks, not a conventional interpreter. Multi-step operations work by pushing `fut` closures onto the expression stack. Understanding and faithfully translating how ev/mev/sched cooperate is the core challenge.

### `where` and Settable References

`(set (cadr x) 'z)` works. The `where` form returns the pair a value lives in plus which half (car or cdr). The evaluator cooperates via `loc` stack entries and the `inwhere` check in `vref`. Getting the interaction between `set`, `where`, `loc`, `inwhere`, `vref`, and the evaluator right is subtle.

### Dynamic Binding

`dyn` puts `bind` entries on the expression stack. Variable lookup must search the stack for these on every reference. Dynamic bindings take precedence over lexical and global.

### Continuations and Protected Expressions

`ccc` captures `s` and `r`. Calling a continuation resumes from that point, but `prot` entries (from `after`) between the current position and the captured one must still be evaluated. `bind` entries must be preserved too. `applycont` in bel.bel handles this.

### Macro Expansion

Macros expand at call time. The evaluator applies the macro function to unevaluated arguments, puts the expansion on the expression stack. The `bquote` macro — the biggest in bel.bel — generates code that generates code, with nesting levels.

### Bootstrapping Order

bel.bel must load top-to-bottom. Dependencies are subtle — `fn` has two cases to avoid infinite recursion with `do`/`reduce`/`def`. Problems during loading likely mean a dependency issue.

### Speed (Or Lack Of It)

This will be slow. Potentially very slow. `(+ 1 2)` traverses the entire number tower as list operations. Loading bel.bel itself involves arithmetic for defining the number tower. We accept this. It's the pure spec running. If it takes minutes to bootstrap, that's the cost of purity and it's fine.

If bootstrapping takes truly unreasonable time (hours), we may need to add native numbers as a pragmatic concession. But we try without first, and measure.

## Testing

The guide contains ~150 REPL examples. Every one is a test case:

```
(id 'a 'a)                             → t
(join 'a 'b)                           → (a . b)
(car '(a . b))                         → a
(no nil)                                → t
(atom \a)                               → t
(all atom '(a b))                       → t
(some atom '((a b) c (d e)))            → (c (d e))
(reduce join '(a b c))                  → (a b . c)
(cons 'a 'b '(c d))                    → (a b c d)
(append '(a b c) '(d e f))             → (a b c d e f)
(map car '((a b) (c d) (e f)))         → (a c e)
(+ 8 5)                                → 13
(sort < '(5 1 3 2 4))                  → (1 2 3 4 5)
(let x '(a b c) (set (cadr x) 'z) x)  → (a z c)
```

Extract all of these into an automated test suite. The project is correct when they all pass.

The ultimate test: `(bel '(+ 1 2))` — the Bel interpreter running inside our implementation of the Bel interpreter. If that returns `3`, we've given Bel its first turtle.

## What Success Looks Like

bel.bel loads. The standard library works. The REPL examples produce correct output, however long they take. You can type Bel expressions and see correct results.

Every decision we made is documented: what we chose, what alternatives existed, what Graham left open and why we think he left it open.

The result is not a fast language. It's a *faithful* one. And it's the foundation from which a future stage 2 — observing the running spec to discover safe optimizations, verified against bel.bel as the permanent authority — can grow.