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

Operators are ordinary functions with symbol names: `a == b` is `(==) a b`,
and `(==)` is `eq`. See [the grammar](grammar.md) for precedence, minus and
sections.

```text
λ> 1 + 2 * 3 == 7 && "a" < "b"
true
λ> each files | filter (.size > 1000) | sortOn .name
λ> (<+>) = a b -> a * 10 + b
(<+>) : Num a => a -> a -> a
λ> infixl 6 <+>
λ> 1 <+> 2 * 3
16
```

## Methods

| Function | Type |
| --- | --- |
| `eq`, `ne` | `Eq a => a -> a -> Bool` |
| `lt`, `le`, `gt`, `ge` | `Ord a => a -> a -> Bool` |
| `min`, `max` | `Ord a => a -> a -> a` |
| `sort` | `Ord a => [a] -> [a]` |
| `sortOn` | `Ord b => (a -> b) -> [a] -> [a]`, stable |
| `==` `!=` | `eq`, `ne` |
| `<` `<=` `>` `>=` | `lt`, `le`, `gt`, `ge` |
| `+` `-` `*`, `negate` | `Num a => a -> a -> a` |
| `/` `%`, `div` `mod` | `Integral a => a -> a -> a` |
| `<>` | `Semigroup a => a -> a -> a` |
| `&&` `\|\|` | `Bool -> Bool -> Bool`, lazy on the right |
| `>>>` `<<<` | composition: `f >>> g` is `x -> g (f x)` |

```text
λ> sort [[2], [1, 5], [1]]
[[1], [1, 5], [2]]
λ> sortOn (r -> r.size) files
```

## Declaring classes and instances

A class names its methods and their types. An instance gives them for one
type.

```text
λ> class Pretty a where pretty : a -> String
class Pretty
λ> instance Pretty Int where pretty = n -> "#" <> show n
instance Pretty Int
λ> pretty 3
"#3"
λ> f = x -> pretty x
f : Pretty a => a -> String
```

Several methods go in a block, which may span lines:

```text
λ> class Shape a where do
..   area : a -> Int;
..   name : a -> String
.. end
λ> instance Shape Int where do area = n -> n * n; name = n -> "square" end
```

- **Superclasses**: `class Eq a => Hashable a where …`. Every `Hashable`
  type is `Eq`, a context shows only `Hashable a`, and an instance of
  `Hashable` needs one of `Eq` for its type.
- **Contexts**: `instance Pretty a => Pretty [a] where …`. The instance holds
  for `[a]` when `Pretty a` does, and its methods can use `Pretty a`, and the
  instance itself, as `pretty` on the elements and on nested lists.
- **Heads** are a type constructor applied to distinct unknowns: `Int`,
  `String`, `()`, `[a]`, `(a -> b)`. Not `[Int]`, and not a record: records
  are `Eq` and `Show` structurally.
- **A context** constrains only the head's unknowns, so resolution always
  ends.
- **Defaults**: a class block may define a method after its signature. An
  instance that leaves the method out gets the default, checked at the
  instance's type as if it had written it, so a default can use the
  instance's other methods:

  ```text
  λ> class Size a where do size : a -> Int; big : a -> Bool; big = x -> size x > 10 end
  λ> instance Size Int where size = n -> n
  λ> big 20
  true
  ```

  Every method without a default must be defined. A default sees the names
  in scope where the instance is declared.
- **Prelude defaults**: `Eq` needs only `eq`, and `Ord` only `lt`: `ne`,
  `le`, `gt`, `ge`, `min` and `max` follow. They use names that no session
  can rebind.
- **Method types** use the type syntax of [the grammar](grammar.md). A
  method's other unknowns stay as general as the class says, so an
  instance cannot fix them. `Comp`, `Job` and `Stream` cannot appear yet,
  because their effect rows have no syntax.
- **Again**: an instance for the same class and type replaces the old one
  for the lines after it. A binding that already chose a dictionary keeps
  it. A user class declared again replaces the old one and drops its
  instances. The prelude's classes cannot be declared again, but their
  instances can: `instance Show Int where show = n -> "int"`.
- **Prelude methods** are dictionary fields, and an instance may name them by
  their operators: `(==)` is `eq` and `div` is `/`.

`:elab` on a declaration shows the dictionary it would make, and declares
nothing:

```text
λ> :elab instance Show a => Show [a] where show = xs -> "n"
(%instance Show List) = %d5 -> {show = xs -> "n"}
```

## Show

`show : Show a => a -> String`. The prompt prints a value through `Show`
when its type has an instance, and as before otherwise, so a declared
instance changes what the prompt prints:

```text
λ> instance Show Int where show = n -> "int"
λ> {a = 1, b = [true]}
{a = int, b = [true]}
λ> x -> x
<function>
```

## Instances

| Class | Instances |
| --- | --- |
| `Eq` | `Int`, `Bool`, `String`, `()`, `Status`, `Bytes`, `Patch`, `Overlay`, `[a]` when `Eq a`, and records when every field is `Eq` |
| `Ord` | `Int`, `Bool`, `String`, `()`, and `[a]` when `Ord a`, lexicographic |
| `Num`, `Integral` | `Int`. `Integral` provides `Num` |
| `Semigroup` | `String`, `Bytes`, `[a]`, and `Overlay`: `dev <> bench` is one overlay whose later patches win |
| `Show` | `Int`, `Bool`, `String`, `()`, `Status`, `Bytes`, `Patch`, `Overlay`, `[a]` when `Show a`, and records when every field is `Show` |

`Ord a` provides `Eq a`, so a function that uses both shows only `Ord a`.
Functions, computations, streams, stages, collectors and jobs have no `Eq`.
Records have no `Ord`, because a row's fields have no order.

## Records

Two records are equal when they have the same fields and every field is
equal, whatever order the fields were written in.

A function over an open record needs `Eq` for the fields it does not know.
That is `Eq { | r}`: `Eq` on a record whose fields are exactly the row `r`.

```text
λ> f = r -> {x = r.name, y = eq r r}
f : (Eq a, Eq { | b}) => {name : a | b} -> {x : a, y : Bool}
λ> f {name = "n", size = 3}
{x = "n", y = true}
```

A record's dictionary holds its fields' dictionaries. The function takes the
dictionary for the tail, which its caller knows, and extends its fields with
the ones it knows itself:

```text
λ> :elab f = r -> {x = r.name, y = eq r r}
f = %d14 %d15 -> let %d11 = (%instance Eq Record) (%extendFields {name = %d14} %d15.%fields) in …
```

An ambiguous row defaults to the empty row.

## Types and defaulting

- `:t` and binding output show a type's class context:
  `f : Eq a => a -> Bool`. With effect equalities too, both share one context:
  `(Eq a, {| b} = {exec ls}) => …`.
- `x = e` generalises over the context, so `f` above works at every `Eq` type.
- A line that runs needs a concrete instance for each predicate. An unknown
  that a predicate mentions and nothing chooses is ambiguous, and defaults to
  `()`, as in GHCi: `eq [] []` is `true`.
- An integer literal is `Num a => a`, so `1` works at any `Num` type. A
  binding that is not a function keeps one type (the monomorphism
  restriction): `x = 1` is `x : Int`, by defaulting `Num` to `Int`.
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
before elaboration, and `:elab` after it: `:elab e` is the code a line would
run, and `:elab x = e` the code a binding would hold.

```text
λ> :elab eq [1] [2]
let %d10 = (%instance Eq Int) in let %d5 = (%instance Eq List) %d10 in eq %d5 [1] [2]
λ> :elab f = x -> eq x x
f = %d6 x -> eq %d6 x x
λ> :elab p = x -> {a = eq x x, b = lt x x}
p = %d10 -> let %d6 = %d10.%Eq in x -> {a = eq %d6 x x, b = lt %d10 x x}
```

A declared instance's dictionary is code: a record of its methods and its
superclasses' dictionaries, under a parameter for each dictionary its
context needs. `Lambdash.Declare` checks a declaration, and
`elaborateInstance` in `Lambdash.Infer` checks the methods and builds that
code. The classes and instances in scope are a `ClassEnv`: the prelude's,
from `Lambdash.Classes`, and the session's declarations. See
[the design](equality.md).
