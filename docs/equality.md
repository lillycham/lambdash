# Classes, equality and operators: design

This records the design decided with Lilly on 2026-11-04, so that it can be
built in slices. Slices 1, 2a and 2b are built: see [classes](classes.md).

## Decisions

| Question | Decision |
| --- | --- |
| Class kind | **Open**: users declare classes and instances |
| Mechanism | **Dictionary passing**: inference elaborates core, inserting instances as hidden arguments (Wadler–Blott) |
| Built-in classes | `Eq`, `Ord`, `Num`, `Show`, `Semigroup`, written as ordinary classes in a prelude |
| Open records | **A row constraint**, `EqRow r`, pending until the tail is known |
| `Ord` | **Designed with `Eq`**. `Ord` implies `Eq` |
| Operators | **A general operator table**, below; `<>` (Semigroup) instead of `++`, and composition |
| User operators | **Yes, Haskell-style**: symbol names, with `infixl`/`infixr`/`infix` declarations; default `infixl 9` |
| Externals | **Operator characters stay words** in external arguments. Only `\|`, `&` and `;` end a word |
| Minus | **Spacing decides**: `a - b`, `a-b`, `x-1` subtract; `-1` after a space or bracket, with a digit after, is a literal |
| Sections | **Plain and accessor sections**: `(== 1)`, `(1 <)`, and `(.size > 1mb)` for `r -> r.size > 1mb`. `(- 1)` is the section `x -> x - 1` |
| Pipeline | parse → surface (spans) → desugar → core → infer and elaborate → core with dictionaries → eval |

## The operator table

Loosest first. Every operator binds tighter than `|` and looser than
application, so `ls | filter (.size > 1mb) | sort .name` reads as expected.

| Level | Operators | Associativity |
| --- | --- | --- |
| — | `x -> …` lambda body, trailing `&`, `\|` pipeline | as before |
| 1 | `>>>` `<<<` composition | right |
| 2 | `\|\|` | right |
| 3 | `&&` | right |
| 4 | `==` `!=` `<` `<=` `>` `>=` | none: `a == b == c` is an error |
| 5 | `<>` | right |
| 6 | `+` `-` | left |
| 7 | `*` `/` `%` | left |
| 9 | user operators, by default | left |
| 10 | application | left |
| — | `.` select and accessor, with no spaces | |

- `-1`, after a space or a bracket and before a digit, is a negative literal:
  `f -1` is `f (-1)`. Otherwise `-` is subtraction: `x-1`, `x - 1`.
- `.` cannot be composition, because it is select.
- `--` starts a comment, so no operator begins with it.
- `$` stays out of expression syntax: it is the variable sigil in external
  arguments.

Proposed, not yet confirmed:

- **Operator characters**: `! % & * + - / < = > ? @ ^ | ~ :`. Not `#`
  (flake references), `$` (variables) or `.` (select). Reserved: `=`, `->`,
  `<-`, `|`, `&`, `::`, a lone `:`, and anything that starts with `--`.
- **Definitions**: `(<+>) = a b -> …`, and a fixity line `infixl 6 <+>` that
  lasts for the session and affects later lines, as bindings affect head
  resolution now.
- **Semigroup instances**: `String`, lists, `Bytes`, and `Overlay`: the plan
  calls overlays an ordered, non-commutative monoid, so `dev <> bench` is one
  overlay whose later patches win.

## Consequences

- **Classes are values in core.** A class declaration gives a dictionary
  type; an instance gives a dictionary value; a constraint `C a =>` becomes a
  dictionary parameter. Eval then needs no new machinery beyond records of
  methods.
- **Return-polymorphic methods work**, so numeric literals can be
  `fromInt : Num a => Int -> a` later, with defaulting.
- **Operators desugar to methods.** `a == b` is `(==) a b`, a section is a
  lambda, and an accessor section is a lambda over a record. All of that is
  `Lambdash.Desugar`; the checker sees only application.
- **Records.** `Eq {a : Int, b : String}` holds when every field type is
  `Eq`, decided structurally rather than by a user instance. An open row
  carries `EqRow r` until the tail is known.

## Still to decide

1. **Defaulting for `Num`**: `Eq` and `Ord` default an ambiguous unknown to
   `()`, as GHCi does. `Num` would default to `Int`.
2. **Declaration syntax** for `class` and `instance`, at the prompt and in a
   startup file, and what redeclaring one in a session means.
3. **Coherence**: overlapping instances, and instances that a later line
   adds after a binding already chose one.
4. **Instances for built-in types that users may want to replace**, such as
   `Show` for records.

## Slices

1. Desugar pass, behaviour-preserving. Done: `Lambdash.Syntax`,
   `Lambdash.Desugar`.
2. Class and instance core, dictionary-passing elaboration, the prelude's
   `Eq` and `Ord` for built-in types, with functions such as `eq` before any
   operator syntax. Split: **2a, done**: base types and lists, superclasses,
   defaulting ambiguous unknowns to `()`, one context in `:t`. **2b, done**:
   records, with `EqRow r` written `Eq { | r}`, and row evidence: a record's
   dictionary carries its fields' dictionaries.
3. The operator table, minus by spacing, sections and accessor sections,
   user operators and fixity lines.
4. `Num` and literal defaulting, `Show`, `Semigroup`, and class and
   instance declarations at the prompt.
