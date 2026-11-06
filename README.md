# λsh

A functional shell with composable environments: static inference with
inferred effect rows, structured streams, and Nix environments as values that
can be used, scoped, inspected and removed without nesting shells.

```text
λ> use nixpkgs#ffmpeg
λ [nixpkgs#ffmpeg]> ls | filter (e -> startsWith "mov" e.ext) | .name | collect
λ [nixpkgs#ffmpeg]> with .#bench do cabal bench end
```

The syntax is provisional.

## Run

```bash
nix run github:lillycham/lambdash
```

The flake's `packages.default` is the `lambdash` binary, so a system
configuration can install it from the flake input.

## Build and test

```bash
nix develop -c cabal run lambdash
```

```bash
nix develop -c test/run.sh
```

`test/run.sh` builds, then runs every unit suite and every pty suite with a
fresh state directory. `--no-nix` skips the suite that builds a flake.
`LAMBDASH_TRACE_TTY=1` traces terminal and job-control calls to stderr.

## An evaluator written in λsh

[`examples/metacircular.lsh`](examples/metacircular.lsh) evaluates a small,
strict, lexically scoped subset of λsh. Run it from the repository root:

```bash
nix develop -c cabal run exe:lambdash -- --no-init < examples/metacircular.lsh
```

Guest programs are `Expr` values, not source strings. The file defines
the evaluator and runs examples, including recursive factorial, lexical
capture, higher-order functions, and guest errors.

| Guest syntax | AST |
| --- | --- |
| integers, booleans | `IntLit n`, `BoolLit b` |
| variables | `Var "x"` |
| `x -> body` | `Lam "x" body` |
| `f x` | `App f x` |
| `let x = value in body` | `Let "x" value body` |
| `let rec f = x -> rhs in body` | `LetRec "f" "x" rhs body` |
| `if c then a else b` | `If c a b` |
| integer `+`, `-`, `*`, `==` | `Bin Plus a b`, `Bin Minus a b`, `Bin Times a b`, `Bin IntEq a b` |

`eval : (String -> Result Value) -> Expr -> Result Value` is the core.
An environment is a lookup function; a closure stores its parameter,
body AST, and definition environment. `LetRec` ties a recursive lookup
function with the host's `let rec`. `run` evaluates in the empty environment
and renders the result.

Applications evaluate the function, then the argument, then the body.
Let bindings and binary operands are strict too; `If` evaluates only its
chosen branch. Unbound variables and invalid operand types return `Err`
rather than fail in the host.

The host checks the evaluator's types, but guest type errors are detected
at runtime. There is no guest parser, type inference, data declaration,
pattern matching, mutual recursion group, or shell effect support.
Like the host, the evaluator can diverge.

## Documentation

| Topic | |
| --- | --- |
| [grammar](docs/grammar.md) | the syntax, and its sharp edges |
| [pipelines](docs/pipelines.md) | externals, bytes, exit status |
| [streams](docs/streams.md) | lists, lines, filters, `collect`, `pipefail` |
| [effects](docs/effects.md) | effect rows and the preflight |
| [blocks](docs/blocks.md) | `do … end`, `with` |
| [environments](docs/environments.md) | overlays, `use`, `unuse`, `:env` |
| [nix](docs/nix.md) | `pkg`, `dev`, flake references, GC roots, `andThen` |
| [directories](docs/directories.md) | path arguments, `cd`, `ls`, `useLocal`, `allow` |
| [jobs](docs/jobs.md) | Ctrl-Z, `&` and job handles, `wait`, `jobs`, `fg`, `bg` |
| [nix notes](docs/nix-notes.md) | Nix behaviour, checked against Nix 2.34 |
| [classes](docs/classes.md) | classes and instances, `Eq`, `Ord`, `Show`, and how dictionary passing works |
| `:help` | at the prompt: an overview, `:help NAME`, `:help TOPIC`, `:help CLASS` |
| [types](docs/types.md) | annotations `(e : t)` and signatures `f : t` |
| [data](docs/data.md) | `data` types, `case` and patterns, `let rec` |
| [startup](docs/startup.md) | `init.lsh`, run before the first prompt |
| [scripts](docs/scripts.md) | `lambdash FILE`, `-x`, and what a script does on Ctrl-C |
| [modules](docs/modules.md) | `import`, and what a module may hold |
| [editors](docs/editors.md) | `lambdash --lsp`, for VS Code, Helix and Neovim |
| [equality](docs/equality.md) | design: classes, `Eq`, `Ord`, operators |

## Licence

λsh uses the MIT licence. See [LICENSE](LICENSE).
