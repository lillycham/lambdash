# Classes, equality and operators: design

There is no `==` yet. This records the design decided with Lilly on
2026-11-04, so that it can be built in slices. Nothing here is built, except
the desugar pass it depends on.

## Decisions

| Question | Decision |
| --- | --- |
| Class kind | **Open**: users declare classes and instances |
| Mechanism | **Dictionary passing**: inference elaborates core, inserting instances as hidden arguments (Wadler–Blott) |
| Built-in classes | `Eq`, `Ord`, `Num`, `Show`, written as ordinary classes in a prelude |
| Open records | **A row constraint**, `EqRow r`, pending until the tail is known |
| `Ord` | **Designed with `Eq`**. `Ord` implies `Eq` |
| Operators | **A general operator table**, not only comparisons |
| Minus | **Spacing decides**: `a - b`, `a-b`, `x-1` subtract; `-1` after a space or bracket, with a digit after, is a literal |
| Sections | **Plain and accessor sections**: `(== 1)`, `(1 <)`, and `(.size > 1mb)` for `r -> r.size > 1mb` |
| Pipeline | parse → surface (spans) → desugar → core → infer and elaborate → core with dictionaries → eval |

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

1. **The operator table**: which operators, their precedence and
   associativity, and where `|`, `&`, application and `->` sit. Whether users
   can declare operators and fixities.
2. **Defaulting**: what an ambiguous `Num a` or `Eq a` becomes at the prompt,
   as `[] == []` would be.
3. **Declaration syntax** for `class` and `instance`, at the prompt and in a
   startup file, and what redeclaring one in a session means.
4. **Coherence**: overlapping instances, and instances that a later line
   adds after a binding already chose one.
5. **Display**: how `:t` shows class constraints beside effect
   qualifications, as in `(Eq a, {| r} = {exec ls}) => …`.
6. **Instances for built-in types that users may want to replace**, such as
   `Show` for records.

## Slices

1. Desugar pass, behaviour-preserving. Done: `Lambdash.Syntax`,
   `Lambdash.Desugar`.
2. Class and instance core, dictionary-passing elaboration, the prelude's
   `Eq` and `Ord` for built-in types, with functions such as `eq` before any
   operator syntax.
3. The operator table, minus by spacing, sections and accessor sections.
4. `Num` and literal defaulting, `Show`, and user declarations at the prompt.
