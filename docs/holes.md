# Holes

`_` where an expression goes is a hole. A line with a hole does not run.
The checker tells what type would fit there instead:

```text
λ> title = "hello"
λ> stringLength _
hole: String
  stringLength _
               ^
  fits: title, (display _), (max _ _), (min _ _), (show _)
λ> ls "." | _ | encodeLine
hole: {name : String, type : String, size : Int, ext : String} -> String
  ls "." | _ | encodeLine
           ^
  fits: .ext, .name, .type, display, show
```

- The type is what the hole meets: the parameter it is passed to, the
  operator beside it, or the stages around it. It keeps its class context,
  as in `show _`, which is `Show a => a`.
- A hole between `|`s is a function from the items to the items after it,
  because `|` makes a stage of a function.
- Each hole has its own report, so `stringLength _ + _` gives a `String`
  and an `Int`.
- A binding with a hole fails, so it binds nothing.
- `_` is still a lambda's parameter, `_ -> 1`, and a pattern, `case x of _ -> …`.
  In a command, `echo _` passes the word `_`.

## What fits

Under each hole are the texts that would fit it, up to six:

- A fit is a text that, put in the hole, makes the line check. The checker
  decides, with the whole line around the hole.
- First come names: those the line binds itself, such as a lambda's
  parameter, then the session's own, then the builtins. A hole that takes a
  record also gets its fields, such as `.name`.
- Then refinements: a function whose result fits, with holes for its
  arguments, such as `(show _)`. Put one in, and its new holes say what
  they want.
- A fit never needs an instance that does not exist. With other holes in
  the line, though, an instance that depends on what fills them is not
  checked yet.

In an editor, each hole is a problem at its place, with its type and what
fits. Each fit is also a quick fix that puts it in the hole.

## Filling a pipeline: `:fill`

`:fill` takes a line with one hole between pipes, and finds chains of
stages for it. Each chain shows the first lines the whole line would
print with it, on your real data:

```text
λ> :fill ls "." | _ | map .name | encodeLine
 1  sortedOn .name            a.txt · b.txt · notes.md · src
 2  sortedOn .type            src · a.txt · b.txt · notes.md
 3  sortedOn .size            src · b.txt · a.txt · notes.md
 4  sortedOn .ext             src · notes.md · a.txt · b.txt
 5  reverse                   src · notes.md · b.txt · a.txt
 6  sortedOn .type | reverse  notes.md · b.txt · a.txt · src
 7  sortedOn .size | reverse  notes.md · a.txt · b.txt · src
 …
λ> :pick 7
in the history, ↑ to edit: ls "." | sortedOn .size | reverse | map .name | encodeLine
```

- A chain has up to three steps: a stage such as `sorted` or `unique`, one
  that takes a number, one that takes a function of the items with a field
  (`map .name`, `sortedOn .size`), a test (`filter isEmpty`), or a fit for
  the hole. The checker checks each chain in the line.
- In `take 5` and `drop 5`, the 5 stands in for a number. Change it after
  you pick.
- The source, everything before the hole, runs once. Its first 200 items
  go to a buffer, and each chain runs on the buffer. So a preview never
  runs the source again.
- The source and each chain run only if all they do is read files: their
  effect row is at most `{fs}`. Otherwise `:fill` still lists the chains
  but runs nothing, and says what the source needs, such as
  `{exec git}`.
- Two chains that print the same lines are one answer: the shorter stays.
  A chain that prints nothing here comes last.
- `:pick N` puts that line in the history: ↑ brings it back to edit or
  run. The line itself never runs from `:fill`.
