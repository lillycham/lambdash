# Editors

`lambdash --lsp` is a language server on stdin and stdout. It checks a `.lsh`
file as a startup file would run it, line by line, and reports each line that
does not parse or does not check. It never runs a line: no command starts, no
flake activates and no file changes.

## What it reports

- A parse error, at its line and column.
- A type error, on the whole of its line, or of its block. The checker does
  not keep locations in its errors yet.

The file starts from a new session: the startup file does not run first, so
a name that only `init.lsh` binds is a command in another file.

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

VS Code needs an extension for a new language. There is none yet.
