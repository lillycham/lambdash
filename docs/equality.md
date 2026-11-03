# Equality: a design note

There is no `==` yet. Filters use helpers such as `startsWith` and `contains`.
Lilly's direction, from 2026-11-03: equality should be constrained, as in
`(==) : Eq a => a -> a -> Bool`, not available at every type.

This note records what that needs, so that the shape can be chosen
deliberately. Nothing here is built.

## Why not an unconstrained `==`

`== : a -> a -> Bool` would typecheck `f == g` on functions, computations,
streams and jobs, which have no sensible equality. It would have to fail at
run time, which is what static inference is there to prevent.

## What `Eq a` needs

λsh has no type classes. The checker already carries qualified types, but only
for effect-row equalities (`TQualified [EffConstraint] Ty`), so the plumbing
for constraints that travel through schemes, `generalise` and `instantiate`
exists. A class constraint would add a second kind of constraint to it.

Choices to make:

1. **Which types are `Eq`.** Likely: `Int`, `Bool`, `String`, `()`, `Status`,
   `Bytes`, `Overlay`, `Patch`, and lists and records of `Eq` types. Not
   functions, `Comp`, `Stream`, `Stage`, `Collector` or `Job`.
2. **Records.** Row-polymorphic records need `Eq` for every field, including
   the unknown tail: `{a : Int | r}` is `Eq` only if the fields in `r` are. One
   option is a row constraint, `EqRow r`, that solves as the tail fills in.
3. **When unresolved constraints are errors.** At the prompt, as with effect
   rows, an unresolved `Eq a` on a run line could default or be refused.
4. **How evaluation compares.** Structural equality on `Value` is enough while
   only built-in types are `Eq`. User-defined instances would need dictionary
   passing, which is a larger change to `Eval`.
5. **Display.** `:t` would show `Eq a => a -> a -> Bool`, beside effect
   qualifications.
6. **Ordering.** `Ord` for `<`, `sort .size` and so on would follow the same
   pattern, so it is worth designing both at once.

## Smaller alternative

Monomorphic helpers, `eqInt`, `eqString` and so on, need no checker change.
They are easy to replace later, but awkward to use.
