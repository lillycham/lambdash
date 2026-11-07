# Editors

`lambdash --lsp` is a language server on stdin and stdout. It checks a `.lsh`
file as a startup file would run it, line by line, and reports each line that
does not parse or does not check. It never runs a line: no command starts, no
flake activates and no file changes.

## What it reports

- A parse error, at its line and column.
- A hole, `_`, at its place, with the type that would fit there and what
  fits it. Each fit is a quick fix that puts it in the hole. See
  [holes](holes.md).
- A type error, under the expression it is about. A mismatch is under the
  argument that does not fit, or under the expression that does not fit its
  annotation. A missing instance, such as `Num Bool`, is under the use
  that asked for it: the `1` of `1 + true`. An error the checker cannot
  place, such as a failed `import`, is on its whole line or block.

## Hover

Hover on a name to see its type at that point in the file, as `:t` shows
it: a later binding of the same name changes it. A name that a line uses
has the type it had before that line, so in `x = if x then 1 else 2` the
`x` on the right is the earlier one. The `x` on the left, where the line
binds it, has the new type.

A name that its own line binds, such as a lambda's parameter, a `let`, a
pattern variable or an `x <- e`, shows its type in that line, with its
class context: in `f = x -> x + 1`, `x` is `Num a => a`. On the binder
itself, the type comes from a use of it, so a binder that nothing uses shows
nothing. Hover shows names only, not other parts of an expression.

Columns count characters. The server converts them to the UTF-16 units of
the protocol, so a line with a tab or an emoji puts each mark in its place.

The file starts from a new session: the startup file does not run first, so
a name that only `init.lsh` binds is a command in another file.

The server follows `import` lines as the REPL does. A relative path starts at
the file's directory, and a file that is not saved yet starts at the
server's directory. The module's names, classes, data types, instances and
fixities are then known, and hover shows a prefixed name such as
`p.padRight`. The server checks the module's definitions but does not run
them. A module that does not load is a problem on its `import` line, and the
problem names the line in the module. The server reads each module again when
the file changes, so an edit to a module shows at the next change.

A name that nothing binds is an external command, as at the prompt. So `y`
alone is not an error, even when `y = ...` failed above it.

## Inlay hints

A binding without a signature shows its type after its name, faintly, as
`f : Num a => a -> a = x -> x + 1`. A binding with a signature shows
nothing. In VS Code, a double click on the hint writes the signature as a
line above the binding. The server offers that only where the signature
checks, so the file stays clean. A `<-` binding and an operator have a hint
but no signature to write.

## Outline

The outline lists what the file defines, in order: bindings with their
types, `let rec` members, data types with their constructors, classes with
their methods, instances, and imports under a prefix. A name bound twice is
there twice. A line that fails defines nothing.

## Go to definition

A name goes to the definition it sees: the latest one before its line, so
in `x = x + 1` the `x` on the right goes to the earlier `x`. A binding's own
name, a `let rec` member and a constructor go to where they are named. A
name from a module goes to the module's file, through the modules it imports
if need be.

A name that a line binds inside itself goes to its binder there, by the
scopes of the line as written: a lambda's parameter, a `let`, a pattern
variable, an `x <- e` in a block, and a `$x` in a command. An inner binder
shadows an outer one, so in `x -> (x -> x)` the last `x` goes to the second.
A `let` is not recursive, so in `let x = x + 1 in x` the first `x` on the
right is the one from before. A builtin goes nowhere.

## Completion

Completion offers the names that the line sees, with their types: the
session before the line, as hover uses. After a dot it offers a module's
names (`p.pa` gives `p.padRight`) or a record's fields, as Tab does at the
prompt. Without a dot it also offers the reserved words, and the words of the
line or block itself, such as its parameters, which no session holds.

## Code lens

Above each line or block that runs, as the file runs, the server shows the
effects it may have, as its type gives them: `{console}`, `{exec git, fs}`,
or `pure` for a value that the REPL prints. A definition, a declaration and
a line that fails show nothing. A name whose binding failed shows
`{exec y}`, because it is the command `y`, as at the prompt.

In VS Code the lens also has `▶ Run`, which sends the line or block to the
λsh prompt, as Shift+Enter does. Nothing runs twice, but the names the line
needs must be in that prompt already.

## Helix

In `languages.toml`:

```toml
[language-server.lambdash]
command = "lambdash"
args = ["--lsp"]

[[language]]
name = "lambdash"
scope = "source.lambdash"
file-types = ["lsh"]
comment-token = "--"
language-servers = ["lambdash"]
```

## Neovim

```lua
vim.filetype.add({ extension = { lsh = "lambdash" } })
vim.lsp.config("lambdash", {
  cmd = { "lambdash", "--lsp" },
  filetypes = { "lambdash" },
})
vim.lsp.enable("lambdash")
```

## VS Code

The extension is in `editors/vscode`. It gives `.lsh` files highlighting,
comments, brackets and `do`/`end` indentation, and starts `lambdash --lsp`
for diagnostics, hover, inlay hints, the outline, go to definition,
completion and code lenses.

The flake builds it, with its `node_modules`, as `vscode-extension`. With
home-manager:

```nix
programs.vscode.profiles.default.extensions = [
  inputs.lambdash.packages.${pkgs.stdenv.hostPlatform.system}.vscode-extension
];
```

To try it without installing it, from the repository root:

```bash
nix build .#vscode-extension
code --extensionDevelopmentPath="$PWD/result/share/vscode/extensions/lillycham.lambdash"
```

| Setting | Default | |
| --- | --- | --- |
| `lambdash.path` | `lambdash` | the program, as a name on `PATH` or an absolute path |

`Lambdash: Restart the Language Server` restarts it, as a change to
`lambdash.path` does.

Three commands run code:

| Command | Key | |
| --- | --- | --- |
| `Lambdash: Run File` | ▶ in the editor title | saves the file, then runs `lambdash FILE` in a new terminal |
| `Lambdash: Send Selection to the Prompt` | Shift+Enter | types the selection, or the line, at a λsh prompt |
| `Lambdash: Open a Prompt` | | shows that prompt |

Run File starts in the workspace folder, or in the file's directory when
the file is outside a workspace, and closes the terminal of the last run.
The terminal shows the script's exit status. See [scripts](scripts.md).

Send Selection uses one prompt, kept between sends, so a session builds up
line by line. With no selection it sends the line under the cursor and moves
to the next line, so Shift+Enter steps through a file. A `do` block sent
whole continues at the prompt, as when typed.

The highlighting is a TextMate grammar, so it reads words, not the parser's
syntax. A `--` with a space on each side is a comment, so
`git commit -- file` shows `-- file` as one. The server does not send
semantic tokens yet.
