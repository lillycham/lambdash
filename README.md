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
| [equality](docs/equality.md) | design: classes, `Eq`, `Ord`, operators |
