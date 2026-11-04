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
- Effect rows and record tails are not rigid: the binding fills them in. So
  `c : Int -> Comp ()` with `c = n -> print n` is `Int -> Comp {console} ()`,
  and the preflight still sees its commands.
- What a signature leaves open defaults as a line does.
- The first binding that type-checks uses the signature up. One that fails
  leaves it waiting. A binding of another name in between does not touch it.
- In the startup file, signatures are quiet, like bindings.
