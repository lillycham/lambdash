# Nix resolvers

Two resolvers build overlays from Nix. Resolving builds and changes nothing
in the session. `use` commits the result.

```text
pkg :: String -> Comp {exec nix} Overlay              -- a package's outputs on PATH
dev :: String -> Comp {exec bash, exec nix} Overlay   -- a devshell, hook run once
```

```text
λ> e <- pkg "nixpkgs#ffmpeg"          -- build now
e : Overlay
λ> use e                              -- activate now, or later, or never
λ [nixpkgs#ffmpeg]> andThen (dev ".#") use
λ [nixpkgs#ffmpeg .#]>
```

The two differ because `nix shell` and `nix develop` differ. `pkg ".#"` is
`packages.default`. `dev ".#"` is `devShells.default`, and falls back to the
environment for building `packages.default`. Flake references are strings for
now. The overlay's name is the reference.

Nix runs as an ordinary foreground job, so its progress reaches the terminal
and Ctrl-C stops a build. A failed build reports its status and binds nothing.

## `andThen`

```text
andThen :: Comp {| a} b -> (b -> Comp {| c} d) -> Comp {| a + c} d
```

Run a computation, then the computation its result chooses, with the union of
both effect rows. `andThen (pkg "nixpkgs#jq") use` resolves and activates on one
line.

`|` does not bind. `c | f` maps `f` over the result, so `pkg "x" | use` is a
computation that returns a computation. Binding in `|` would depend on whether
`f` returns a computation, which a polymorphic `f` decides only after
instantiation, and evaluation has no types to decide with.

## `pkg`

`pkg ref` runs `nix build ref --print-out-paths` and prepends `<output>/bin` to
`PATH` for each printed output, first output first. This is everything
`nix shell` changes, including outputs with no `bin` directory.

## `dev`

`dev ref` runs `nix print-dev-env ref` and sources the script in
`bash --norc --noprofile`, with the effective environment as the outer
environment. The script joins `PATH` and `XDG_DATA_DIRS` onto the outer values,
and runs `shellHook` last. Then `dev` diffs `env -0` against the outer
environment:

| After | Patch |
| --- | --- |
| unchanged | none |
| `x:old` | `prepend name "x"` |
| `old:x` | `append name "x"` |
| new or different | `set` |
| removed | `unset` |

A prepend replays onto whatever the outer value is at the time, so a devshell
composes with a `pkg` used before or after it.

Never carried: `TMPDIR`, `TMP`, `TEMP`, `TEMPDIR`, `NIX_BUILD_TOP` (the
resolve's own `mktemp` directory), `SHELL` (Nix's bash), `PWD`, `OLDPWD`,
`SHLVL`, `_`, `out`, `name` and `shellHook`.

**The hook runs once, when `dev` runs.** `use`, `unuse` and replay only apply
patches. An overlay records what the hook set, not what it did: a daemon it
started keeps running, and a path it created can go stale. Run `dev` again to
refresh.

Bash functions do not survive into an overlay. `dev` lists any the hook
defined:

```text
λ> d <- dev ".#"
dev .#: the hook defined 2 functions, not carried over: lsh_one lsh_two
```

## GC roots

With a root directory, each resolve roots what it built:

- `pkg` passes `--out-link`, and `dev` passes `--profile`.
- Roots live in `$XDG_STATE_HOME/lambdash/roots/<pid>/`, by default under
  `~/.local/state`.
- The directory is removed when the shell exits, and directories of dead
  sessions are removed at start.

Roots last for the session rather than while an overlay is active. An overlay
is a value, and `use e` can activate it again after `unuse`. Dropping the root
on `unuse` would let a garbage collection delete what `e` still names. The
cost is that a long session keeps everything it resolved.

## Tests

`test/NixTests.hs` covers the diff, `env -0` parsing, output paths and hook
marking without Nix. `test/nix.exp` runs real Nix against `test/nix-fixture`,
whose lock reuses the main flake's nixpkgs:

```bash
expect test/nix.exp "$(cabal list-bin lambdash)"
```

Run it from the repository root. The first run builds the fixture.

## Not yet

- Bare flake references (`use nixpkgs#ffmpeg`).
- `use --local`, `with` and trust: Prototype 5.
- Invalidation when `flake.lock` changes.
- A devshell is resolved in one directory, and `NIX_LDFLAGS` names
  `$PWD/outputs/out/lib` from there.
