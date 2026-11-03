# Nix behaviour, checked

The plan describes the Nix behaviour that Prototype 4 relies on from memory.
This file records what Nix actually did.

Checked with Nix 2.34.6 on x86_64-linux, against nixpkgs `885ba4d` (the rev in
`flake.lock`), using a scratch flake with `packages.default = hello`,
`packages.jq = jq`, a `devShells.default` with a `shellHook`, and a
`devShells.bench` that sets `CC`.

## Summary

| Claim in the plan | Verdict |
| --- | --- |
| `nix shell` gives a package's outputs on `PATH` | True, and nothing else changes |
| `nix develop` gives the build environment | True |
| `.#` means `packages.default` for shell, `devShells.default` for develop | True, and develop falls back to `packages.default` |
| `nix print-dev-env --json` is a flat snapshot, not a diff | True, and it does not run `shellHook` |
| `dev` can run `shellHook` once in bash and diff the environment | True: source the non-JSON `print-dev-env` script |
| `PATH`, `XDG_DATA_DIRS` should become `Prepend` | True, and `nix develop` does exactly that |
| An overlay's store paths are unrooted | True, and an indirect root fixes it |

## `pkg`: what `nix shell` does

`nix shell .#jq -c env` differs from the outer environment in `PATH` only.
It prepends `<output>/bin` for each output in `meta.outputsToInstall`:

```text
$ nix eval --json .#jq.meta.outputsToInstall
["bin","man"]
$ nix shell .#jq -c printenv PATH
/nix/store/…-jq-1.8.2-bin/bin:/nix/store/…-jq-1.8.2-man/bin:<outer PATH>
```

- `jq-man/bin` does not exist. Nix adds it anyway.
- `nix build .#jq --no-link --print-out-paths` prints exactly those outputs,
  one per line. So `pkg` is: build, then one `Prepend ":" "<path>/bin"` on
  `PATH` per printed path.

So `pkg` overlays are `PATH` prepends only, and they compose without conflict.

## `dev`: what `nix develop` does

### `print-dev-env --json`

```json
{ "bashFunctions": { "name": "body", … },
  "variables": { "NAME": { "type": "exported" | "var" | "array", "value": … } } }
```

For a two-line `mkShell`: 121 variables (75 exported, 25 plain, 21 arrays)
and 98 bash functions, all from stdenv.

- **The hook does not run.** `shellHook` is an ordinary exported variable.
  Nothing the hook exports appears, and its `mkdir` did not happen.
- **`PATH` is a full replacement.** It holds the devshell's own entries, but no
  entry from the user's `PATH`.
- **Sandbox values leak through.** `TMPDIR=/build`, `HOME=/homeless-shelter`,
  `NIX_BUILD_TOP=/build`, `TERM=xterm-256color`, `TZ=UTC`, `NIX_LOG_FD`.
- **Only `exported` variables reach a child.** Plain variables and arrays are
  bash state.

### `nix develop -c env`, compared with the JSON

`nix develop` repairs the snapshot before running the command:

| Variable | JSON | `nix develop` |
| --- | --- | --- |
| `PATH`, `XDG_DATA_DIRS` | devshell only | devshell `:` outer value |
| `HOME`, `TERM` | sandbox value | outer value kept |
| `TMPDIR`, `TMP`, `TEMP`, `TEMPDIR`, `NIX_BUILD_TOP` | `/build` | a fresh `mktemp -d -t nix-shell.XXXXXX` |
| `TZ`, `NIX_LOG_FD`, `NIX_ENFORCE_PURITY` | set | removed |
| `out` | the env's store path | `$PWD/outputs/out` |
| `NIX_LDFLAGS` | `-rpath <store>/lib …` | `-rpath $PWD/outputs/out/lib …` |
| `NIX_GCROOT` | absent | the `nix-shell-env` store path |
| `shellHook` exports | absent | present: the hook ran |

The fresh temp directory **is not removed** when the command exits.

`out` and `NIX_LDFLAGS` depend on the directory `dev` ran in, so a cached
devshell overlay is only exact for that directory.

### Running the hook

`nix print-dev-env .#` without `--json` prints a bash script. Its last lines
are:

```bash
PATH="$PATH${nix_saved_PATH:+:$nix_saved_PATH}"
XDG_DATA_DIRS="$XDG_DATA_DIRS${nix_saved_XDG_DATA_DIRS:+:$nix_saved_XDG_DATA_DIRS}"
export NIX_BUILD_TOP="$(mktemp -d -t nix-shell.XXXXXX)"
export TMP="$NIX_BUILD_TOP"  # and TMPDIR, TEMP, TEMPDIR
eval "${shellHook:-}"
```

Sourcing it in `bash --norc --noprofile` runs the hook once, joins `PATH` and
`XDG_DATA_DIRS` onto the outer values, and leaves the hook's functions defined
(100 functions after sourcing: the 98 from stdenv plus the hook's two). This is
the direnv approach, and Nix already does the hard part.

**A plan for `dev`:**

1. `nix print-dev-env --profile <root> <ref>`, which builds, roots the
   environment and prints the script in one step.
2. In `bash --norc --noprofile`, with the effective environment as the outer
   environment: `source` the script, then print `env -0` and `declare -F`.
3. Diff against the outer environment. For each variable:
   - `after == x <> ":" <> before` gives `Prepend ":" x`, and likewise `Append`;
   - a new or different value gives `Set`;
   - a removed name gives `Unset`.
4. Drop `TMPDIR`, `TMP`, `TEMP`, `TEMPDIR` and `NIX_BUILD_TOP`. They name the
   `mktemp` directory of one resolve, which would go stale. Probably drop
   `SHELL` too: Nix sets it to its own bash, and other programs read it.
5. Report hook functions as the functions after sourcing minus the names in
   `print-dev-env --json`'s `bashFunctions`.

### Conflicts are mostly identical values

Every devshell sets `CC`, `CXX`, `LD`, `AR`, `NIX_CC`, `NIX_BINTOOLS`,
`NIX_CFLAGS_COMPILE` and about 70 other variables from stdenv. Two devshells
on one stdenv set them to the same values. **The ⚠ flag should fire only when
an overlay changes a value set by an earlier one**, or it fires on every
second devshell.

### An attribute named like a stdenv variable is overwritten

`devShells.bench = mkShell { CC = "bench-cc"; }` gives `CC=gcc` inside
`nix develop`, because the cc-wrapper setup hook exports `CC` after the
attribute. The plan's `⚠ CC` example would need `CC` set in a `shellHook`.

## Flake references

| Command | `.#` | `.#name` |
| --- | --- | --- |
| `nix shell` | `packages.<system>.default` | `packages.<system>.name` |
| `nix develop` | `devShells.<system>.default`, then `packages.<system>.default` | `devShells.<system>.name`, then `packages.<system>.name` |

`nix develop .#jq`, with no `devShells.jq`, builds `jq-1.8.2-env`: the
environment for building jq. With `devShells.default` removed, `nix develop .#`
builds `hello-2.12.3-env`. So `dev nixpkgs#ffmpeg` means "the environment for
building ffmpeg", as the plan says, and it does not fail.

## GC roots

Before rooting, `nix-store -q --roots` on `jq-1.8.2-bin` printed nothing.

| Action | Root |
| --- | --- |
| `nix build .#jq --out-link roots/jq` | `roots/jq-bin`, `roots/jq-man`: one link per output, with a suffix |
| `nix print-dev-env --profile roots/dev .#` | `roots/dev -> dev-1-link -> …-nix-shell-env` |
| `nix-store --add-root roots/jq2 --realise <path>` | `roots/jq2` |

- Each is an indirect root: Nix records a link in
  `/nix/var/nix/gcroots/auto/` to the symlink.
- **Deleting the symlink drops the root.** Nix removes the stale `auto` entry
  during its next root scan.
- The `nix-shell-env` closure includes the devshell's packages, so one
  `--profile` root keeps a whole devshell alive.
- A multi-output `--out-link roots/jq` creates `roots/jq-bin` and
  `roots/jq-man`, not `roots/jq`. `unuse` must remove every link it made, so
  it should record them rather than recompute them. `--add-root` per output
  path avoids the suffixes.

`nix develop` sets `NIX_GCROOT`, but that root lasts only as long as the
`nix develop` process. It does not help a long-lived overlay.
