# λsh expression grammar

This is the grammar of `Lambdash.Parser`. The syntax is provisional. If the
parser and this file disagree, one of them has a bug.

The notation is EBNF: `{ x }` is zero or more, `[ x ]` is optional, `|` is
choice. Lowercase names are syntax rules and uppercase names are tokens.

## Syntax

Whitespace and comments can appear between any two items in a syntax rule.

```ebnf
program     = expr EOF ;

expr        = let | if | lambda | pipeline ;

let         = "let" name "=" expr "in" expr
            | "let" "rec" recgroup "in" expr ;
recgroup    = name "=" lambda { "and" name "=" lambda } ;   (* a lambda, perhaps annotated *)
if          = "if" expr "then" expr "else" expr ;
lambda      = param { param } "->" expr ;
param       = IDENT | "_" ;
pipeline    = stage { "|" stage } [ "&" ] ;
stage       = operand { OP operand } ;            (* resolved by fixity *)
operand     = lambda | external | application ;
name        = IDENT | "(" OP ")" ;
application = postfix { slot } { postfix } ;
external    = HEAD { ARG } ;

block       = "do" statement { ";" statement } [ ";" ] "end" ;
case        = "case" expr "of" arm { ";" arm } [ ";" ] "end" ;
arm         = pattern "->" expr ;
pattern     = UPPER { patom } | patom ;
patom       = "_" | LOWER | UPPER | INT | STRING | "true" | "false" | "(" ")" | "(" pattern ")" | precord ;
precord     = "{" [ pfield { "," pfield } ] "}" ;
pfield      = LOWER [ "=" pattern ] ;
statement   = IDENT "<-" expr | name "=" expr | expr ;
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
atom        = block | with | case | REF | literal | record | list | accessor | IDENT
            | "(" expr ")" | "(" OP ")" | "(" OP stage ")" | "(" expr OP ")"
            | "(" expr ":" type ")" ;
literal     = INT | STRING | "true" | "false" | "(" ")" ;
record      = "{" [ field { "," field } ] "}" ;
field       = IDENT "=" expr ;
list        = "[" [ expr { "," expr } ] "]" ;
```

## Tokens

```ebnf
IDENT       = LETTER { LETTER | DIGIT | "_" | "'" } - RESERVED ;
RESERVED    = "let" | "in" | "true" | "false" | "do" | "end" | "with"
            | "if" | "then" | "else" | "rec" | "and" | "case" | "of" ;
INT         = [ "+" | "-" ] DIGIT { DIGIT } ;
STRING      = a Haskell string literal ;
COMMENT     = "--" { any character except newline } ;
OP          = OPCHAR { OPCHAR } - OPRESERVED ;
OPCHAR      = ! % & * + - / < = > ? @ ^ | ~ : ;
OPRESERVED  = "=" | "->" | "<-" | "|" | "&" | "::" | ":" | "--" { OPCHAR } ;

HEAD        = WORD ;     (* a path, or a word that starts with an unbound name *)
ARG         = "$" IDENT | STRING | WORD ;
WORD        = { "\\" any character | any character except space and \ | " $ ; & } ;
              (* a ) } or ] that closes no bracket of the word itself ends it *)
REF         = { LETTER | DIGIT | . _ - / : ~ + @ ? = & # % } ;   (* with at least one # *)
```

A reserved word ends where a character that can continue an `IDENT` does not
follow it. So `letter` is an `IDENT`, not `let` and `ter`.

`LETTER` and `DIGIT` are Unicode: `letterChar` and `alphaNumChar`.

## Choices that the grammar does not show

- `lambda` and `application` can start with the same tokens. The parser tries
  `lambda` first, and goes back if no `->` arrives. So `f x` is an application,
  and `f x -> x` is a lambda with two parameters.
- `if c then a else b`: between `if` and `else`, the words `then` and `else`
  end an external, as `end` does in a block; quote them to pass them. The
  `else` branch goes as far right as it can. Both branches have one type,
  but two computations, jobs or streams join their effect rows: the line may
  do either, so the preflight checks both. Only the chosen branch runs.
- The body of a `lambda` or a `let` goes as far right as it can. A lambda
  argument needs parentheses: `f (x -> x)`.
- `let rec f = x -> ... and g = y -> ... in e`: every name of the group is in
  scope in every bound expression and in `e`, so `g x` in `f` is a call even
  before `g` is defined. Each bound expression must be a lambda, as in OCaml:
  evaluation is strict, so `let rec x = x + 1` could only loop. Inside the
  group a name has one type; the group then generalises as one, as a Haskell
  binding group does, and its members share a class context. A recursive
  call at another type is a type error: there is no polymorphic recursion.
  Recursion adds no effects: a `countdown` that prints and calls itself
  has the row `{console}`, not `{console | e}`.
  `let` without `rec` is still not recursive.
- `application` goes to the left: `f x y` is `(f x) y`.
- `pipeline` goes to the left: `a | b | c` is `(a | b) | c`.
- Application has higher precedence than `|`: `a | f x` is `a | (f x)`.
- `postfix` goes to the left: `r.a.b` is `(r.a).b`.
- `.a.b` means `r -> r.a.b`.
- Inside `INT`, no space can follow the sign.
- Operators bind looser than application and tighter than `|`, by the
  fixity table below. Two operators of one precedence must agree on
  associativity, and a non-associative one cannot chain: `a == b == c` is
  an error, "cannot mix == and == without parentheses".
- Minus is decided by spacing. `-` with a digit after it, after a space or
  an opening bracket, is a negative `INT`: `f -1` is `f (-1)`. Otherwise it
  subtracts: `x-1`, `x - 1`. In parentheses, `(-1)` is a number and `(- 1)`
  the section `x -> x - 1`.
- `(op e)` is a right section, `x -> x op e`, and `(e op)` a left section,
  `op e`. `(op)` is the operator as a value.
- An operator expression in parentheses whose leftmost operand is an
  accessor is an accessor section: `(.size > 1)` is `r -> r.size > 1`.
- Inside `external`, operator characters are ordinary word characters:
  `git log -n 1` and `a+b` are words. `|`, `&` and `;` end a word, so
  `make && y` is `&&` applied to `make` and `y`.
- A lone `|` pipes and a lone `&` backgrounds; `||` and `&&` are operators.
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
| `a op b` | `(op) a b` |
| `(op e)` | `x -> (op) x e` |
| `(e op)` | `(op) e` |
| `(.a op e)` | `r -> r.a op e` |

Everything else, including `|`, `with`, `&` and running a statement, has its
own type rule in core, because what it means depends on types.

`:core e` shows what `e` desugars to, without running it. It writes an
operator applied to two arguments infix, with parentheses around each
operand that is itself an operator expression: `:core 1 + 2 * 3` is
`1 + (2 * 3)`. Core has words of
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

The REPL parses one `line` at a time. `:t`, `:core`, `:elab`, `:q` and `:env` are reserved words
in this rule only. A line may span several input lines while a `do` or a `case` is open.

```ebnf
line        = [ ":q" | ":help" [ NAME ] | ":t" expr | ":core" expr | ":elab" [ name "=" ] expr | ":env" [ NAME ] | command
              | ":elab" declaration | declaration
              | fixity | name ":" [ context "=>" ] type
              | name "=" expr | "let" "rec" recgroup | name "<-" expr | expr ] EOF ;
fixity      = ( "infixl" | "infixr" | "infix" ) DIGIT OP { OP } ;

declaration = "class" [ context "=>" ] UPPER LOWER "where" members(signature | definition)
            | "instance" [ context "=>" ] UPPER tatom "where" members(definition)
            | "data" UPPER { LOWER } "=" UPPER { tatom } { "|" UPPER { tatom } } ;
context     = pred | "(" pred { "," pred } ")" ;
pred        = UPPER tatom ;
members(x)  = x | "do" x { ";" x } [ ";" ] "end" ;
signature   = name ":" type ;
definition  = name "=" expr ;

type        = btype [ "->" type ] ;
btype       = "Comp" [ row ] tatom | "Job" [ row ] tatom
            | "Stream" [ row ] tatom tatom tatom
            | "Stage" tatom tatom | "Collector" tatom tatom
            | UPPER tatom { tatom } | tatom ;            (* a declared type: Maybe Int *)
tatom       = LOWER | UPPER | "(" ")" | "(" type ")" | "[" type "]"
            | "{" [ IDENT ":" type { "," IDENT ":" type } ] [ "|" LOWER ] "}" ;
row         = "{" [ label { "," label } ] [ "|" LOWER ] "}" ;   (* only when the arguments follow *)
label       = "exec" WORD | "console" | "env" | "fs" | "cwd" ;
command     = "jobs" | "fg" [ JOB ] | "bg" [ JOB ] | "allow" | "deny" ;
JOB         = [ "%" ] DIGIT { DIGIT } ;
```

- A `command` is a command only when nothing binds its word, like an external.
  See [jobs](jobs.md) and [directories](directories.md).

- `x = e` names a value and runs nothing.
- `let rec f = x -> ...` (and `and`, without `in`) binds every name of the group.
  A waiting signature decides a member's type, as for `x = e`.
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
- `case`, `of`, `rec` and `and` are reserved. In a `case` scrutinee, `of` ends a
  command line, as `then` does after `if`. See [data types](data.md).
- `class` and `instance` start a declaration only before a capital or a
  parenthesis, so either is still a name. `where` is not reserved. In a
  type, a lowercase name is an unknown and a capitalised one a constructor.
  See [classes](classes.md).
- A `fixity` line sets how later lines parse its operators, for the rest of
  the session. `infixl`, `infixr` and `infix` are not reserved: the line is a
  fixity only with a digit and operators after the word.

## Fixities

Loosest first. An operator with no fixity is `infixl 9`.

| Level | Operators | Associativity |
| --- | --- | --- |
| 1 | `>>>` `<<<` | right |
| 2 | `\|\|` | right |
| 3 | `&&` | right |
| 4 | `==` `!=` `<` `<=` `>` `>=` | none |
| 5 | `<>` | right |
| 6 | `+` `-` | left |
| 7 | `*` `/` `%` | left |
| 9 | anything else | left |
