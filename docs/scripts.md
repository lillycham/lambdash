# Scripts

```bash
lambdash build.lsh
```

`lambdash FILE` runs a file as a script, then exits. Each line runs in turn,
as at the prompt, and its output appears as it comes. A definition, such as
`x = …`, `data` or `import`, shows nothing, as in a startup file.

```text
$ cat build.lsh
version = "0.1"
echo "building $version"
cabal build
$ lambdash build.lsh
building 0.1
…
```

## When a line fails

The first line that fails stops the script. Its place and message go to
stderr, and the status is 1. A type error also shows its line, with a mark
under the expression it is about:

```text
$ cat fail.lsh
x = 1
print (x + 2)
y = x + 1
print (stringLength x)
$ lambdash fail.lsh
3
fail.lsh:4: type mismatch: String and Int
  print (stringLength x)
                      ^
$ echo $?
1
```

A line fails when it does not parse or check, or when it raises an error
while it runs. A program that exits with a status other than 0 does not fail
the line: its status is a value, as at the prompt. `:q` ends the script with
status 0.

## Options

| Option | |
| --- | --- |
| `-x` | print each line to stderr before it runs, after `+ `, as `bash -x` does |
| `--no-init` | has no effect on a script, which never reads the startup file |

Options go before the file. A script cannot take arguments yet:
`lambdash build.lsh release` is refused, so that a later version can give
them to the script without changing what the line means.

## What a script does not do

- It does not read `init.lsh`, as a bash script does not read `.bashrc`.
- No flake activates when it starts or changes directory, even an allowed
  one, as direnv does not act for a bash script. A script does the same for
  everyone who runs it. To use a devshell, say so: `with .# do … end`.

## Signals

A script keeps the signals it started with, and its jobs' signals reach it,
as a bash script's do:

- Ctrl-C while a program runs stops the program and then the script, which
  dies of the same signal, so its status is 130. A shell that started it sees
  it as interrupted, and bash drops the rest of that command line.
- Ctrl-Z while a program runs stops the program and the script. `fg` in the
  shell that started it continues both.
