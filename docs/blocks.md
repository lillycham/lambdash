# Blocks

`do … end` runs statements in order. Statements are separated by `;`.

```text
λ> do cabal build; cabal test end
λ> r <- do s <- sh -c "exit 3"; pure s end
r : Status
λ> do x <- pure "hi"; echo $x end
hi
```

| Statement | Meaning |
| --- | --- |
| `x <- e` | run `e` and name its result for the statements after it |
| `x = e` | name a value; nothing runs |
| `e` | run `e`; its result is dropped, except from the last statement |

A block is a computation, so `=` names it and a bare line or `<-` runs it:

- Its value is the last statement's result. The last statement must be an
  expression.
- A stream statement reads the terminal, writes its output, and gives its
  return value, as at the prompt.
- A failed command does not stop the block, as in other shells. Bind the
  status with `<-` to decide.
- Its effect row is the union of the statements' rows, and the preflight
  checks every command in the block before the first statement runs.
- A name bound by `<-` has one type in the block, like a lambda parameter. A
  name bound by `=` is generalised, like `let`.
- A name a statement binds is a function for the statements after it, not a
  command.

## Lines and words

- A `;` may come before `end`.
- Newlines between statements are spaces, so a block can span lines:

  ```text
  λ> do print 1;
  .. print 2 end
  ```

  The REPL asks for another line while a `do` is unclosed. Only whole words
  outside string literals count.
- `;` ends a bareword everywhere, so `echo a;b` is two statements in a block
  and a parse error outside one.
- **Inside a block, a bare `end` ends a command line.** `do echo end end`
  runs `echo` with no arguments. To pass the word, quote it: `echo "end"`.

## `with`

```text
with overlay do … end
```

`with` runs a block with one more environment layer, on top of the session's,
for the dynamic extent of the block. It is Reader's `local`, not a closure:
a function returned from a `with` block does not carry the layer.

```text
λ> o = overlay "o" [set "CC" "clang"]
λ> with o do printenv CC end
clang
λ> printenv CC
gcc
```

- The prompt does not change. Shadowing that the layer causes warns once on
  stderr.
- **`with` discharges the commands it can see.** The body runs with the
  layer, so the session's `PATH` says nothing about it, and
  `:t with o do ffmpeg end` has no `exec ffmpeg`. Each pipeline still
  resolves its programs before it spawns anything, against the effective
  environment and the layer together. A command that only a parameter knows,
  as in `c -> with o do c end`, stays in the row.
- `use` inside a `with` block changes the session, below the layer.
- A stopped `with` block keeps its layer when `fg` resumes it.

`with (pkg "nixpkgs#ffmpeg") do …` does not typecheck, because `pkg` returns a
computation. Use `andThen (pkg "nixpkgs#ffmpeg") (o -> with o do … end)`, or
resolve it first with `<-`.
