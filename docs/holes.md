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
