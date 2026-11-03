# Environments

An environment is a value: a named, ordered list of patches. `use` adds one to
the session and `unuse` removes it. No nested shell starts, and the process
environment of λsh never changes: each spawn gets the effective environment as
its `envp`.

```text
λ> dev = overlay "dev" [set "CC" "clang", prepend "PATH" "/opt/dev/bin"]
dev : Overlay
λ> use dev
λ [dev]> printenv CC
clang
λ [dev]> unuse "dev"
λ>
```

`=` names an overlay and activates nothing. `use` is a computation, so a bare
line or `<-` runs it.

## Patches

| Builtin | Type | Effect on the variable |
| --- | --- | --- |
| `set name value` | `String -> String -> Patch` | replace |
| `unset name` | `String -> Patch` | remove |
| `prepend name value` | `String -> String -> Patch` | `value:old`, or `value` if unset |
| `append name value` | `String -> String -> Patch` | `old:value`, or `value` if unset |
| `prependWith sep name value` | `String -> String -> String -> Patch` | `value<sep>old` |
| `appendWith sep name value` | `String -> String -> String -> Patch` | `old<sep>value` |
| `overlay name patches` | `String -> [Patch] -> Overlay` | |

Patches in one overlay apply in order, so an overlay can set a variable and
then prepend to it. `PATH` is an ordinary variable.

## The session

| Builtin | Type | |
| --- | --- | --- |
| `use o` | `Overlay -> Comp {env} ()` | activate, on top |
| `unuse name` | `String -> Comp {env} ()` | deactivate |
| `environments` | `Comp {env} [String]` | active names, oldest first |

`env` is a new effect label: reading or changing the session's environments.

- **Later wins.** The effective environment is the process environment with
  each active overlay applied, oldest first.
- **Using an active name replaces it**, and the new one goes on top.
- **Removing re-folds.** No patch needs an inverse.
- **Transactional.** A line's changes are kept only if the whole line
  succeeds. `unuse "nope"` fails, and so changes nothing.
- **Resolution follows the environment.** The preflight and the spawn both
  search the effective `PATH`, so an overlay that prepends to `PATH` changes
  which program runs, and `set "PATH" "/nowhere"` makes `echo` not found.
- **Every stage of one pipeline sees one environment**, read when the
  pipeline starts.
- **A computation reads the session when it runs, not when it is built.**
  `g = printenv X`, then `use`, then `g` sees the overlay.

## Shadowing

When an overlay replaces a value that an earlier overlay chose, the line warns
once, and the prompt carries a flag for as long as it lasts:

```text
λ [dev]> use (overlay "bench" [set "CC" "gcc"])
warning: bench shadows CC from dev
λ [dev bench ⚠ CC]>
```

- Only a different value counts. Every Nix devshell sets `CC` and about 70
  other stdenv variables, usually to the same values (see `nix-notes.md`), so
  flagging names would flag every second devshell.
- `prepend` and `append` extend a value, so they never shadow.
- Overriding the process environment is not shadowing.
- `unuse` clears the flag.

## Introspection

`:env` lists active environments and their patches, oldest first, and marks
the patches that shadow:

```text
λ [dev bench ⚠ CC]> :env
dev
  set "CC" "clang"
  prepend "PATH" "/opt/dev/bin"
bench
  set "CC" "gcc"  ⚠ shadows dev
```

`:env NAME` shows a variable's effective value, its base value and each layer
that touched it:

```text
λ [dev bench ⚠ CC]> :env CC
CC = "gcc"
  base  "cc"
  dev  set "CC" "clang"
  bench  set "CC" "gcc"
```

## Not yet

- `pkg` and `dev`, which build overlays from Nix, and GC roots: Prototype 4.
  `nix-notes.md` has the checked behaviour they rely on.
- `with`, `use --local` and trust: Prototype 5. The runtime handle that
  computations receive is where `with` will add a local layer.
- `unuse` takes a string, because a bare word in expression position is a
  variable.
- No `export`: the base is the environment λsh started with.
