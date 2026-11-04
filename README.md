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
| [equality](docs/equality.md) | design: classes, `Eq`, `Ord`, operators |
