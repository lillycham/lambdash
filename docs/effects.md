# Effect rows

Effects describe what an action requires when it runs.
No annotation is required. Use `:t` to see the inferred row.
Normal value and binding output omits the row.

```text
:t echo hello
Stream {exec echo} Bytes Bytes Status

:t echo hello | cat
Stream {exec cat, exec echo} Bytes Bytes Status

:t x -> echo $x
String -> Stream {exec echo} Bytes Bytes Status

:t print 1
Comp {console} ()

:t pure 1
Comp Int
```

An omitted row is empty. `console` records the output effect of `print`.
`exec name` records an external command requirement.
Rows attach to both `Comp` and `Stream`.

## Latent actions

An action remains a value until a bare prompt line or `<-` runs it.
Its row follows it through functions, records, bindings and return values.
Only the outer action's row enters the REPL preflight.

```text
job = missing-command
f = x -> job
:t f
a -> Stream {exec missing-command} Bytes Bytes Status
```

These lines require no executable. `f 1` checks `missing-command` before it runs.
`pure job` has no immediate effects: it returns an action without a spawn.
`c <- pure job` binds that action; a later `c` checks its requirements.

A missing command stops the whole action before any effect occurs.
The runtime retains its concrete plan check as well, because names can change after preflight.

## Open rows

Rows are sets. Pipeline composition unions their labels and open tails.
A repeated requirement occurs once. Independent tails remain independent.
Generalisation and instantiation include effect unknowns, as they include type and record-row unknowns.
An open union appears as `{exec cat | a + b}`.
The checker retains an equality that does not determine a row uniquely.
For example, `{exec cat | e} = {exec cat}` permits `e` to be empty or contain `exec cat`.
It does not permit any other effect.

`:t` shows these conditions before the type:

```text
({exec cat | a} = {exec cat}) => Stream { | a} Bytes Bytes b -> Stream {exec cat | a} Bytes Bytes Status
```

The conditions follow the type through functions, records, local definitions and `<-` results.
Each use of a polymorphic value gets fresh unknowns and conditions.
A later argument can resolve a condition. A contradiction stops inference before any action runs.
An unused local definition still constrains any enclosing monomorphic unknown that it mentions.

Preflight can run an action when its conditions prove one exact closed outer row.
The row may have a tail internally: `{exec cat | e}` above always equals `{exec cat}`.
If the outer requirements remain ambiguous, the REPL reports an unresolved effect row.
The checker does not choose an arbitrary permitted set or assume that unnamed effects are absent.

Exact equality can still reject a monomorphic function that accepts actions with different effects.
Effect subtyping remains future work.

Computed command heads and `{exec *}` have no syntax yet.
Environment handlers and row discharge belong to the later environment slices.
