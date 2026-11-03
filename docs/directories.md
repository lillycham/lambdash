# Directories

## Paths in builtin arguments

Some builtins declare an argument a path. There a bareword is a string, as in
an external command, so `ls src` and `cd ..` need no quotes. This is the
signature-directed parsing proposed in the plan, which Nushell also does.

| Builtin | Slot | Default |
| --- | --- | --- |
| `cd` | path | `~` |
| `ls` | path | `.` |

- In a path slot, `$x` is a variable, `"…"` is a string, and `( … )` is any
  expression. A bareword is a string, so `cd x` means the directory `x`.
- A missing argument takes the default. `ls | filter f` lists `.`. A bare
  `cd` means `cd ~` anywhere, also in `:t`, so the function itself is
  `x -> cd $x`.
- A builtin's signature applies only while it is still the builtin. Rebinding
  `ls`, or naming a lambda parameter `ls`, makes it an ordinary function.
- Other builtins take expressions, so `filter .ok` still works.
- `ls` is a builtin, so `ls -la` means the directory `-la`. For the program,
  give a path: `/bin/ls -la`.

## `cd`

```text
cd : String -> Comp {cwd} ()
```

`cd -` goes back. `~` and `~/…` expand. `PWD` and `OLDPWD` follow, and later
spawns inherit the directory.

`cd` changes the process at once. A line that fails after a `cd` keeps the
new directory.

## `ls`

```text
ls : String -> Stream {fs} Bytes {name : String, type : String, size : Int, ext : String} ()
```

`ls` streams one record per entry, sorted by name. `type` is `file`, `dir` or
`link`. `ext` is the part after the last dot, or empty. A directory's size is
0.

```text
λ> ls | filter (e -> startsWith "h" e.ext) | .name | collect
{items = ["Main.hs"], status = (), statuses = []}
```

`fs` and `cwd` are provisional effect labels, a first answer to the plan's
open question about filesystem effects. The preflight ignores them.

## Directory-scoped environments

```text
useLocal : Overlay -> Comp {env} ()
```

`useLocal o` activates `o` in the current directory and below it, for the
rest of the session. Activation is a function of the state and the
directory:

```text
active cwd = filter (inScope cwd) overlays
```

Leaving the tree deactivates it, and coming back restores it. No hook runs
again:

```text
λ> cd ~/projects/akyuu
λ> andThen (dev .#) useLocal
λ [.#]> cd ~/projects/raupeka
.# deactivated
λ> cd ~/projects/akyuu
.# restored
λ [.#]>
```

`useLocal nixpkgs#jq` means `pkg`, as `use` does. `use` of the same name
makes it global again, and `unuse` removes it. `:env` shows where each local
overlay applies, and whether that is here.

The plan's `use --local` cannot parse, because `--` starts a comment in
expression position.

## Approval

When the shell enters a directory with a `flake.nix`, or starts in one, it
checks whether that flake is allowed:

| State | What happens |
| --- | --- |
| allowed, same hash | `dev .` resolves once, its hook runs, and it is used locally as `<dir>#` |
| not allowed | one message per session; nothing runs |
| allowed, then `flake.nix` or `flake.lock` changed | the overlay is dropped, and the flake needs approval again |

| Command | |
| --- | --- |
| `allow` | approve this directory's flake at its current hash, and activate it |
| `deny` | withdraw approval, and deactivate it |

Typing `dev .#` is consent, as typing `nix develop` is. Entering a
directory is not, because a cloned repository's `shellHook` would run. So an
approval names the SHA-256 of `flake.nix` and `flake.lock`, from
`nix hash file`.

Approvals are kept in `$XDG_STATE_HOME/lambdash/allowed`, by default under
`~/.local/state`, as one `hash directory` line each.

Only the directory itself is checked, not its parents. `allow` and `deny` are
commands only when nothing binds them, like `jobs` and `fg`.
