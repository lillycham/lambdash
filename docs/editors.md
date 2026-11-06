# Editors

`lambdash --lsp` is a language server on stdin and stdout. It checks a `.lsh`
file as a startup file would run it, line by line, and reports each line that
does not parse or does not check. It never runs a line: no command starts, no
flake activates and no file changes.

## What it reports

- A parse error, at its line and column.
- A type error, on the whole of its line, or of its block. The checker does
  not keep locations in its errors yet.

## Hover

Hover on a name to see its type at that point in the file, as `:t` shows
it: a later binding of the same name changes it. A name that a line uses
has the type it had before that line, so in `x = if x then 1 else 2` the
`x` on the right is the earlier one. The `x` on the left, where the line
binds it, has the new type. A name that its own line binds, such as a
lambda's parameter or a pattern variable, shows nothing for now. Hover has
no types for parts of an expression yet.

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
for diagnostics and hover.

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

`λsh: Restart the language server` restarts it, as a change to
`lambdash.path` does.

The highlighting is a TextMate grammar, so it reads words, not the parser's
syntax. A `--` with a space on each side is a comment, so
`git commit -- file` shows `-- file` as one. The server does not send
semantic tokens yet.
