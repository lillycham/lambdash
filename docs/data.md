# Data types, `case` and `let rec`

## Data types

```text
λ> data Maybe a = Nothing | Just a
data Maybe a
λ> data List a = Nil | Cons a (List a)
data List a
λ> Cons 1 (Cons 2 Nil)
Cons 1 (Cons 2 Nil)
λ> :t Just
a -> Maybe a
```

A `data` line declares a nominal type and its constructors. Each constructor
is a function of its fields, or a value when it has none. A field can be any
written type that mentions only the type's parameters, including the type
itself. An omitted effect row in a field is empty, as in a method's type.

The checks:

- Every type that a field names exists, with as many arguments as it takes.
- A field mentions no unknown that is not a parameter.
- The builtin types (`Int`, `String`, `Comp` and the rest) cannot be declared.
- The constructors of one type are distinct.

Constructors are ordinary names: a later binding can shadow one. A pattern
always means the constructor.

### A type declared again

A type declared again is a new type. The old one retires under a name that
no written type can spell, `T#1`, then `T#2`, and so on:

```text
λ> data T = A Int
data T
λ> x = A 1
x : T
λ> data T = A String
data T
λ> :t x
T#1
λ> case x of A s -> stringLength s end
type mismatch: T#1 and T
  T#1 is an earlier declaration of T
  case x of A s -> stringLength s end
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

- Bindings keep the old type, and its constructors and instances go with it.
  A constructor that the new declaration does not reuse still makes old
  values, and a pattern with it still matches them.
- A written type, in a signature or an instance head, names the newest `T`.
  The new type starts with no instances.
- `:help T#1` shows the old declaration.
- An import whose module declares `T` retires the session's `T` in the same
  way. Importing a module again retires the types of the first import, so
  values from the two imports do not mix. A module cannot declare one type
  twice.

`:help List` shows the declaration. There is no `deriving` yet: write the
instances you need.

```text
λ> instance Show a => Show (Maybe a) where show = m -> case m of Nothing -> "none"; Just x -> "just " <> show x end
```

## `case`

```text
case e of p -> a; q -> b end
```

The first arm whose pattern matches gives the value. A pattern is one of:

| Pattern | Matches |
| --- | --- |
| `_` | anything |
| `x` | anything, and binds `x` in the arm |
| `Just p`, `Cons p q`, `Nothing` | that constructor, with patterns for its fields |
| `0`, `-1`, `"s"`, `true`, `()` | that value |
| `{name = p, size}` | a record with at least these fields; `size` alone binds `size` |

A capitalised name is a constructor, and any other name binds. An integer
pattern is an `Int`: there is no class for matching a number of any type.
A name binds once in one pattern.

A record pattern is open: `{size = 0}` matches any record with a `size`
field, and its type is `{size : Int | r}`. A field written alone, as in
`{size}`, binds its own name. A field is in one pattern once.

```text
λ> size = r -> case r of {size = 0} -> "empty"; {size} -> show size end
size : {size : Int | a} -> String
λ> size {name = "a", size = 3}
"3"
```

Every arm's pattern has the scrutinee's type, and the arms join as the
branches of an `if` do: two computations, jobs or streams join their effect
rows. Only the chosen arm runs.

The checker refuses a `case` that does not cover every value, and names an
example that no arm matches:

```text
λ> case m of Just x -> x end
the case does not cover Nothing
λ> case xs of Nil -> 0; Cons x Nil -> x end
the case does not cover Cons _ (Cons _ _)
λ> case n of 0 -> "zero" end
the case does not cover _
```

For coverage a record is one constructor whose fields are all the labels
that the arms name. A label that an arm leaves out is a wildcard there.

Integers and strings never have all their values listed, so a `case` on
them needs a catch-all. Booleans and `()` can be covered. The check is
Maranget's usefulness algorithm. It does not report an arm that can never
match.

As in a block, `;` and a bare `end` end a command line in an arm, and `of`
ends one in the scrutinee. A `case` that is still open asks for more lines,
as an open `do` does.

## `let rec`

```text
λ> let rec len = xs -> case xs of Nil -> 0; Cons _ rest -> 1 + len rest end
len : Num a => List b -> a
λ> let rec even = n -> if n == 0 then true else odd (n - 1) and odd = n -> if n == 0 then false else even (n - 1)
even : (Eq a, Num a) => a -> Bool
odd : (Eq a, Num a) => a -> Bool
```

`let rec f = x -> … and g = y -> … in e` is the expression; at the prompt,
the same line without `in` binds the group. Every name of the group is in
scope in every bound expression.

- Each bound expression must be a lambda, as in OCaml. Evaluation is strict,
  so `let rec x = x + 1` could only loop.
- Inside the group a name has one type, as a lambda parameter has. The
  group then generalises as one, as a Haskell binding group does, and its
  members share a class context. A recursive call at another type is a type
  error: there is no polymorphic recursion.
- Recursion adds no effects. A recursive call makes the row equation
  `r = {console} + r`, and the checker closes the tail that only recursion
  adds, so `countdown` has the row `{console}` and the preflight knows what
  it runs. A tail that a parameter mentions stays open.
- A waiting signature decides a member's type at the prompt, as for `x = e`.
- `let` without `rec` is still not recursive.

Recursion means that a pure, well-typed expression can loop. Instances could
already recurse through their own methods. A `div` effect for this is an
open question in the plan.
