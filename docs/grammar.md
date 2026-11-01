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
pipeline    = stage { "|" stage } ;
stage       = lambda | external | application ;
application = postfix { postfix } ;
external    = HEAD { ARG } ;
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
atom        = literal | record | accessor | IDENT | "(" expr ")" ;
literal     = INT | STRING | "true" | "false" | "(" ")" ;
record      = "{" [ field { "," field } ] "}" ;
field       = IDENT "=" expr ;
```

## Tokens

```ebnf
IDENT       = LETTER { LETTER | DIGIT | "_" | "'" } - RESERVED ;
RESERVED    = "let" | "in" | "true" | "false" ;
INT         = [ "+" | "-" ] DIGIT { DIGIT } ;
STRING      = a Haskell string literal ;
COMMENT     = "--" { any character except newline } ;

HEAD        = WORD ;     (* a path, or a word that starts with an unbound name *)
ARG         = "$" IDENT | STRING | WORD ;
WORD        = { any character except space and ) } | " $ } ;
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

## REPL lines

The REPL parses one `line` at a time. `:t` and `:q` are reserved words in this
rule only.

```ebnf
line        = [ ":q" | ":t" expr | IDENT "=" expr | IDENT "<-" expr | expr ] EOF ;
```

- `x = e` names a value and runs nothing.
- `x <- e` runs a `Comp a` or `Stream i o r` and binds its return value (`a` or `r`).
- `e` runs if its type is `Comp a` or `Stream i o r`. Otherwise it shows the value.

See [pipelines](pipelines.md) for stream types, byte chunks and exit status.

## Known sharp edges

- There is no infix `-` yet, so `x-1` is `x (-1)`, an application.
- `INT` accepts a `+` sign.
- `true` and `false` are values, so `true` at the prompt does not run a program.
- `lambda` wins over `external`, so `echo a -> b` is a lambda.
- A `"` ends a word, so `a"b"` is two arguments.
