# Holes

`_` where an expression goes is a hole. A line with a hole does not run.
The checker tells what type would fit there instead:

```text
λ> stringLength _
hole: String
  stringLength _
               ^
λ> ls "." | _ | encodeLine
hole: {name : String, type : String, size : Int, ext : String} -> String
  ls "." | _ | encodeLine
           ^
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

In an editor, each hole is a problem at its place, with its type.
