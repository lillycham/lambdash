# Modules

A module is a `.lsh` file that holds only definitions. An import brings its
names into the session.

```text
-- lib/pad.lsh
let rec repeatText = n s -> if n <= 0 then "" else s <> repeatText (n - 1) s
padRight = width s -> s <> repeatText (width - stringLength s) " "
```

```text
λ> pad = import "./lib/pad.lsh"
imported pad.repeatText, pad.padRight
λ> pad.padRight 5 "ab" <> "|"
"ab   |"
λ> import "./lib/pad.lsh"
imported repeatText, padRight
```

- `name = import "path"` brings the module's bindings in as `name.binding`.
- `import "path"` brings them in as they are.
- The path must be in double quotes, and nothing may follow it. Thus
  `import shot.png` is still the external command `import`.
- A relative path starts at the directory of the file that holds the
  import. At the prompt, it starts at the current directory. A path can
  also start with `~/`.

## Only definitions

A module can hold these lines, and no others:

- bindings: `x = e` and `let rec f = … and g = …`
- signatures, fixity lines and declarations (`data`, `class`, `instance`)
- imports of other modules

A line that would run something stops the import, with the module's path and
the line number. This includes `x <- e` and a bare expression:

```text
λ> import "./m.lsh"
/home/me/m.lsh:2: a module holds only definitions, and this line would run
```

So an import never runs code. A binding such as `st = git status` is a
definition: it names the command, and nothing runs until you use it.

An import that fails binds nothing.

## What comes in, and how

| Name | Comes in as |
| --- | --- |
| a binding | `name.x` with a prefix, `x` without |
| an operator, and its fixity | as it is: `<+>` cannot be written with a prefix |
| a data type and its constructors | as they are |
| a class and its methods | as they are |
| an instance | global, as every instance is |

Data types, classes and instances are global, so a prefix does not apply to
them. Constructors stay unqualified too, so that `case` patterns can use them.
An instance that replaces an existing one gives the same warning as at the
prompt (see [classes](classes.md)).

## Scope

- A module starts from the prelude. It cannot see the bindings of the
  session that imports it. Note: an unbound name is an external command,
  so a module that uses `x` from the importer gets the command `x`.
- A module's own imports are private: their bindings do not come along with
  it. Their data types, classes and instances are global, so those do.
- A local name hides a prefix: in `(pad -> pad.size) r`, `pad.size` selects
  from the record `r`.
- The prefix alone is not a value. `pad` by itself is an unbound name.

A prefix is a name, not a record. A record field holds one type, so a field
cannot be polymorphic or need a class. A module's bindings can be both.

An import cycle stops with the chain of paths.

## In the startup file

An import in `init.lsh` starts at the directory of `init.lsh`. An import line
says nothing there, except for its warnings. See [startup](startup.md).
