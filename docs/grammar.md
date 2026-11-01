# λsh expression grammar

This is the grammar of `Lambdash.Parser`. The syntax is provisional. If the
parser and this file disagree, one of them has a bug.

The notation is EBNF: `{ x }` is zero or more, `[ x ]` is optional, `|` is
choice. Lowercase names are syntax rules and uppercase names are tokens.

## Syntax

Whitespace and comments can appear between any two items in a syntax rule.

```ebnf
program     = expr EOF ;

expr        = let | lambda | application ;

let         = "let" IDENT "=" expr "in" expr ;
lambda      = IDENT { IDENT } "->" expr ;
application = postfix { postfix } ;
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
literal     = INT | "true" | "false" | "(" ")" ;
record      = "{" [ field { "," field } ] "}" ;
field       = IDENT "=" expr ;
```

## Tokens

```ebnf
IDENT       = LETTER { LETTER | DIGIT | "_" | "'" } - RESERVED ;
RESERVED    = "let" | "in" | "true" | "false" ;
INT         = [ "+" | "-" ] DIGIT { DIGIT } ;
COMMENT     = "--" { any character except newline } ;
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
- `postfix` goes to the left: `r.a.b` is `(r.a).b`.
- `.a.b` means `r -> r.a.b`.
- Inside `INT`, no space can follow the sign.

## REPL lines

The REPL parses one `line` at a time. `:t` and `:q` are reserved words in this
rule only.

```ebnf
line        = [ ":q" | ":t" expr | IDENT "=" expr | IDENT "<-" expr | expr ] EOF ;
```

- `x = e` names a value and runs nothing.
- `x <- e` runs `e`, which must have a type `Comp a`, and binds the result.
- `e` runs if its type is `Comp a`. Otherwise it shows the value.

## Known sharp edges

- There is no infix `-` yet, so `x-1` is `x (-1)`, an application.
- `INT` accepts a `+` sign.
