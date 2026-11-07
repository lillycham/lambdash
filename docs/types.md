# Type annotations

Types are written as in [class declarations](classes.md):

```text
a -> b                       a function, to the right
Int  String  ()  [a]         constructors and lists
{name : String | r}          a record, open with a tail
Comp a  Job a  Stream i o r  Stage i o  Collector i o
Comp {console} a             an effect row, as :t prints it
Stream {exec git | e} i o r  a row with a tail
Eq a => a -> Bool            a context, in signatures
```

## Effect rows

A row is written right after `Comp`, `Job` or `Stream`, as `:t` prints it.
Its labels are `exec NAME`, `console`, `env`, `fs` and `cwd`, and `| e`
leaves it open. A brace group with `name : type` fields is a record, as
before: `Comp {name : String}` is a computation that gives a record.

- **A closed row is a ceiling.** `Comp {console} ()` allows `console` and
  nothing else; doing less is fine. So `pure ()` fits it and `cd "/"` does
  not.
- **A row left out is open** in signatures and annotations: the binding's own
  row fills it in. `Comp ()` there means whatever the binding does.
- **A tail ties rows together**: `Comp {| e} a -> Comp {console | e} a` adds
  `console` to whatever the argument does.
- **In a class's method types, a row left out is empty**, and a written one
  is the ceiling for every instance. The preflight then knows what a method
  can run from its class alone:

  ```text
  λ> class Greet a where greet : a -> Comp {console} ()
  λ> instance Greet Int where greet = n -> print n
  λ> instance Greet Bool where greet = b -> do print 1; cd "/" end
  effect mismatch: {console, cwd} and {console}
  ```

Only results widen: a function's parameter must match its row exactly.

## In an expression: `(e : t)`

The annotation's unknowns are fresh, so it narrows the type and no more, as
in OCaml:

```text
λ> (1 : Int)
1
λ> :t (x -> x : Int -> Int)
Int -> Int
λ> (true : Int)
type mismatch: Bool and Int
  (true : Int)
   ^^^^
```

## Signatures: `f : t`, then `f = e`

A signature line gives the type of the next binding of that name. Its
unknowns are rigid: the binding must work for every choice of them.

```text
λ> f : Ord a => [a] -> [a]
λ> f = xs -> sort xs
f : Ord a => [a] -> [a]
λ> h : a -> a
λ> h = x -> x + 1
the binding needs Num a, and its signature does not give it
```

- The context gives the binding's dictionaries. A predicate on a rigid
  unknown must follow from it, through superclasses too: `Ord a` gives
  `Eq a`, not the other way round.
- A signature lifts the monomorphism restriction: after `n : Num a => a`,
  `n = 1` is one value per type.
- Effect rows are not rigid: the binding fills them in. So
  `c : Int -> Comp ()` with `c = n -> print n` is `Int -> Comp {console} ()`,
  and the preflight still sees its commands.
- A record's tail is rigid, like an unknown: `n : {name : String | r} -> String`
  must take any record with a name. A binding that reads another field, or
  closes the record, is less general than its signature. A class that the
  binding needs on the tail must come from the context:
  `Show { | r} => {name : String | r} -> String`. (Before 2026-11-05, tails
  were filled in, and a class on a tail defaulted to `()`.)
- What a signature leaves open defaults as a line does.
- The first binding that type-checks uses the signature up. One that fails
  leaves it waiting. A binding of another name in between does not touch it.
- In the startup file, signatures are quiet, like bindings.

## Search by type: `:find t`

`:find` lists the names whose type fits a written type, best first, as
Hoogle does:

```text
λ> :find [Int] -> [Int]
sort : Ord a => [a] -> [a]
λ> :find Int -> Int -> Int
add : Int -> Int -> Int
(*) : Num a => a -> a -> a
…
```

- A name fits when its type is at least as general as the query: you can
  use it where the query's type is wanted. `sort` fits `[Int] -> [Int]`, but
  `stringLength` does not fit `a -> Int`.
- The query is a signature, so it means what `f : t` means. Its unknowns are
  rigid, and its context must give every class that the name needs:
  `:find a -> String` does not find `show`, and `:find Show a => a -> String`
  does.
- An effect row that the query does not write is open: `:find Comp [String]`
  finds `environments : Comp {env} [String]`.
- The arguments may come in another order, for up to four arguments. Such a
  name comes after the others and says `(arguments in another order)`.
- Names that fit as they are come first. Among them, the more specific type
  comes first.
- At most 20 names show, with a count of the rest.
