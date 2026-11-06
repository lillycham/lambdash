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

This is the plan's `use --local`, spelled as a function: `--` would start a
comment in expression position.

## Approval

When the shell enters a directory with a `flake.nix`, or starts in one, it
checks whether that flake is allowed:

| State | What happens |
| --- | --- |
| allowed, same source | the devshell resolves once, its hook runs, and it is used locally as `<dir>#` |
| not allowed | one message per session; nothing runs |
| allowed, then a file of the flake changed | the overlay is dropped, and the flake needs approval again |
| active, then `flake.nix` is gone or cannot be hashed | the overlay is dropped |

| Command | |
| --- | --- |
| `allow` | approve this directory's flake as its source is now, and activate it |
| `deny` | withdraw approval, and deactivate it |

Typing `dev .#` is consent, as typing `nix develop` is. Entering a
directory is not, because a cloned repository's `shellHook` would run.

An approval names the hash of the flake's source as Nix sees it, from
`nix flake metadata`: every file of a path flake, and every tracked file of a
git flake. The devshell is then evaluated from the copy of that source in
the store, so the files that were approved are the files that are read.
Two things are outside the approval: an untracked file of a git flake, and a
file that a hook reads from the working directory when it runs, as in
`source ./hook.sh`. Write `${./hook.sh}` in the flake to bring such a file in.

On each arrival, a quick hash of `flake.nix` and `flake.lock` notices a
change. The source hash is checked once a session, when the devshell
activates.

The overlay's name is the directory's last component and `#`. When another
active directory has that name, more of the path goes in, as in
`work/akyuu#`.

Approvals are kept in `$XDG_STATE_HOME/lambdash/allowed`, by default under
`~/.local/state`, as one `hash directory` line each. Each `allow` and `deny`
holds a lock while it updates the file, so two shells cannot lose each
other's changes. An approval made before the source hash does not match, so
the shell asks again.

Only the directory itself is checked, not its parents. `allow` and `deny` are
commands only when nothing binds them, like `jobs` and `fg`.
