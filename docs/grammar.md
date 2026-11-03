# λsh expression grammar

This is the grammar of `Lambdash.Parser`. The syntax is provisional. If the
parser and this file disagree, one of them has a bug.

The notation is EBNF: `{ x }` is zero or more, `[ x ]` is optional, `|` is
choice. Lowercase names are syntax rules and uppercase names are tokens.

## Syntax

Whitespace and comments can appear between any two items in a syntax rule.

```ebnf
program     = expr EOF ;

expr        = let | lambda | pipeline ;

let         = "let" IDENT "=" expr "in" expr ;
lambda      = IDENT { IDENT } "->" expr ;
pipeline    = stage { "|" stage } [ "&" ] ;
stage       = lambda | external | application ;
application = postfix { slot } { postfix } ;
external    = HEAD { ARG } ;

block       = "do" statement { ";" statement } [ ";" ] "end" ;
statement   = IDENT "<-" expr | IDENT "=" expr | expr ;
with        = "with" postfix block ;

slot        = "$" IDENT | STRING | "(" expr ")" | WORD ;
```

## Tight rules

No whitespace can appear inside `postfix` or `accessor`. Whitespace is what
separates `r.a`, a select, from `f .a`, an application.

```ebnf
postfix     = atom { "." IDENT } ;
accessor    = "." IDENT { "." IDENT } ;
```

Whitespace can appear inside the brackets of an atom, as in syntax rules.

```ebnf
atom        = block | with | REF | literal | record | list | accessor | IDENT
            | "(" expr ")" ;
literal     = INT | STRING | "true" | "false" | "(" ")" ;
record      = "{" [ field { "," field } ] "}" ;
field       = IDENT "=" expr ;
list        = "[" [ expr { "," expr } ] "]" ;
```

## Tokens

```ebnf
IDENT       = LETTER { LETTER | DIGIT | "_" | "'" } - RESERVED ;
RESERVED    = "let" | "in" | "true" | "false" | "do" | "end" | "with" ;
INT         = [ "+" | "-" ] DIGIT { DIGIT } ;
STRING      = a Haskell string literal ;
COMMENT     = "--" { any character except newline } ;

HEAD        = WORD ;     (* a path, or a word that starts with an unbound name *)
ARG         = "$" IDENT | STRING | WORD ;
WORD        = { any character except space and ) } ] | " $ ; & } ;
REF         = { LETTER | DIGIT | . _ - / : ~ + @ ? = & # % } ;   (* with at least one # *)
```

A reserved word ends where a character that can continue an `IDENT` does not
follow it. So `letter` is an `IDENT`, not `let` and `ter`.

`LETTER` and `DIGIT` are Unicode: `letterChar` and `alphaNumChar`.

## Choices that the grammar does not show

- `lambda` and `application` can start with the same tokens. The parser tries
  `lambda` first, and goes back if no `->` arrives. So `f x` is an application,
  and `f x -> x` is a lambda with two parameters.
- The body of a `lambda` or a `let` goes as far right as it can. A lambda
  argument needs parentheses: `f (x -> x)`.
- `application` goes to the left: `f x y` is `(f x) y`.
- `pipeline` goes to the left: `a | b | c` is `(a | b) | c`.
- Application has higher precedence than `|`: `a | f x` is `a | (f x)`.
- `postfix` goes to the left: `r.a.b` is `(r.a).b`.
- `.a.b` means `r -> r.a.b`.
- Inside `INT`, no space can follow the sign.
- `external` applies only when nothing binds the head. Bound names are the
  session names, lambda parameters and let names. The parser never reads `PATH`.
- Inside `external`, only spaces separate words, and `--` is an ordinary word.
  A line, a pipe or a closing bracket ends it. A quoted pipe stays in its word.
- A `slot` applies only after a builtin that declares path slots, `cd` and
  `ls`, while it is still the builtin. A missing slot takes its default
  (`~` for `cd`, `.` for `ls`). See [directories](directories.md).
- A `REF` is a String. `use REF` means `do o <- pkg REF; use o end`, and so
  does `useLocal REF`. `with REF block` resolves with `pkg` the same way. See
  [Nix](nix.md).
- The last `statement` of a `block` must be an `expr`. A name a statement
  binds is bound for the statements after it. Inside a `block`, a bare `end`
  ends an external, so quote it to pass it. See [blocks](blocks.md).
- In a list element or record field, a comma also ends an external argument.
  Parentheses reset that rule: `[(echo a,b), echo c]` has two elements.
  Outside these containers, `echo a,b` keeps the comma in its argument.

## Desugaring

The parser builds surface syntax, with a source span on every node.
`Lambdash.Desugar` turns it into a smaller core, which the checker and the
evaluator see:

| Surface | Core |
| --- | --- |
| `x y -> e` | `x -> y -> e` |
| `.a.b` | `r -> r.a.b` |
| `do x <- a; y = b; c end` | bind `x` to the result of running `a`, then `let y = b`, then run `c` |
| `do a; c end` | bind an invented name to running `a`, then run `c` |
| `use REF`, `useLocal REF` | `do o <- pkg REF; use o end` |
| `with REF do … end` | `do o <- pkg REF; with o do … end end` |

Everything else, including `|`, `with`, `&` and running a statement, has its
own type rule in core, because what it means depends on types.

`:core e` shows what `e` desugars to, without running it. Core has words of
its own for the forms that only it has: `bind x <- e; rest`, `run e`, `bg e`
and `with o e`.

```text
λ> :core do x <- f; y = g x; h y end
bind x <- f; let y = g x in run (h y)
λ> :core use nixpkgs#jq
bind %1 <- %pkg "nixpkgs#jq"; run (%use %1)
```

Names that desugaring invents start with `%`, which no identifier can
contain, so they capture nothing. The sugar refers to the builtins through
aliases of the same kind, `%pkg`, `%use` and `%useLocal`.

## REPL lines

The REPL parses one `line` at a time. `:t`, `:core`, `:q` and `:env` are reserved words
in this rule only. A line may span several input lines while a `do` is open.

```ebnf
line        = [ ":q" | ":t" expr | ":core" expr | ":env" [ NAME ] | command
              | IDENT "=" expr | IDENT "<-" expr | expr ] EOF ;
command     = "jobs" | "fg" [ JOB ] | "bg" [ JOB ] | "allow" | "deny" ;
JOB         = [ "%" ] DIGIT { DIGIT } ;
```

- A `command` is a command only when nothing binds its word, like an external.
  See [jobs](jobs.md) and [directories](directories.md).

- `x = e` names a value and runs nothing.
- `x <- e` runs a `Comp a` or `Stream i o r` and binds its return value (`a` or `r`).
- `e` runs if its type is `Comp a` or `Stream i o r`. Otherwise it shows the value.

See [pipelines](pipelines.md) for stream types, byte chunks and exit status.
See [structured streams](streams.md) for lists, line framing, filters and collection.

## Known sharp edges

- There is no infix `-` yet, so `x-1` is `x (-1)`, an application.
- `INT` accepts a `+` sign.
- `true` and `false` are values, so `true` at the prompt does not run a program.
- `lambda` wins over `external`, so `echo a -> b` is a lambda.
- A `"` ends a word, so `a"b"` is two arguments.
- A `;` ends a word everywhere, so `echo a;b` is an error outside a block.
- An `&` ends a word too, and a trailing `&` backgrounds the pipeline before
  it: `a | b &` is `(a | b) &`. See [jobs](jobs.md).
- A bare `cd` means `cd ~`, also under `:t`. The function is `x -> cd $x`.
- `--` starts a comment in expression position, which is why flags such as
  `--local` are not syntax.
