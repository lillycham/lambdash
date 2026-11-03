# Classes

`Eq` and `Ord` are type classes. A function that compares values works at
every type that has an instance, and the checker says which types those are.

```text
λ> :t eq
Eq a => a -> a -> Bool
λ> f = x -> eq x x
f : Eq a => a -> Bool
λ> {a = f 1, b = f "s"}
{a = true, b = true}
λ> eq (x -> x) (x -> x)
no instance: Eq (a -> a)
```

There are no operators yet: comparisons are functions. `==`, `<` and the rest
come with the operator table, as sugar for these.

## Methods

| Function | Type |
| --- | --- |
| `eq`, `ne` | `Eq a => a -> a -> Bool` |
| `lt`, `le`, `gt`, `ge` | `Ord a => a -> a -> Bool` |
| `min`, `max` | `Ord a => a -> a -> a` |
| `sort` | `Ord a => [a] -> [a]` |
| `sortOn` | `Ord b => (a -> b) -> [a] -> [a]`, stable |

```text
λ> sort [[2], [1, 5], [1]]
[[1], [1, 5], [2]]
λ> sortOn (r -> r.size) files
```

## Instances

| Class | Instances |
| --- | --- |
| `Eq` | `Int`, `Bool`, `String`, `()`, `Status`, `Bytes`, `Patch`, `Overlay`, and `[a]` when `Eq a` |
| `Ord` | `Int`, `Bool`, `String`, `()`, and `[a]` when `Ord a`, lexicographic |

`Ord a` provides `Eq a`, so a function that uses both shows only `Ord a`.
Functions, computations, streams, stages, collectors and jobs have no `Eq`.
Records get `Eq` and `Ord` in the next slice, through a row constraint.

## Types and defaulting

- `:t` and binding output show a type's class context:
  `f : Eq a => a -> Bool`. With effect equalities too, both share one context:
  `(Eq a, {| b} = {exec ls}) => …`.
- `x = e` generalises over the context, so `f` above works at every `Eq` type.
- A line that runs needs a concrete instance for each predicate. An unknown
  that a predicate mentions and nothing chooses is ambiguous, and defaults to
  `()`, as in GHCi: `eq [] []` is `true`.
- A rebound method is an ordinary function: after `eq = x -> x`, `eq 1` is `1`.

## How it works

Classes compile to dictionary passing (Wadler–Blott). A class's dictionary is
a record of its methods, and `Ord`'s holds its `Eq` dictionary. Inference
elaborates each line:

- a use of a name whose scheme has predicates is applied to their
  dictionaries: `eq x y` runs as `eq d x y`;
- a generalised binding takes its dictionaries as parameters;
- a predicate on a constructed type is resolved from an instance, and
  `Eq [a]` is the list instance applied to the dictionary for `Eq a`.

Dictionaries and evidence have names no identifier can spell
(`%instance Eq Int`, `%d12`), so they capture nothing. `:core` shows a line
before elaboration. Classes and instances are tables in `Lambdash.Classes`,
so that declarations at the prompt can extend them later. See
[the design](equality.md).
