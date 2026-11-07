# Structured streams

```text
r <- printf "owl\ncat\nbarn owl\n" | lines | filter (contains "owl") | collect
r.items
r.status
```

`r.items` is `["owl", "barn owl"]`. `r.status` is `exit 0`.
Collection does not print the stream items.
The record preserves the producer's exit status.

## Sources, adapters and collectors

| Name | Type without effects | Purpose |
| --- | --- | --- |
| `lines` | `Stage Bytes String` | Frame UTF-8 byte chunks into lines. |
| `filter` | `(a -> Bool) -> Stage a a` | Retain items that satisfy a pure predicate. |
| `map` | `(a -> b) -> Stage a b` | Transform each item. |
| `collect` | `Collector a a` | Gather items and retain the source return value. |
| `take` | `Int -> Stage a a` | Pass the first n items, then stop the stages before it. |
| `drop` | `Int -> Stage a a` | Skip the first n items. |
| `unique` | `Ord a => Stage a a` | Pass the first of each value, in the order they come. |
| `sorted` | `Ord a => Stage a a` | Sort the items, stably. |
| `sortedOn` | `Ord b => (a -> b) -> Stage a a` | Sort the items by a key, stably. |
| `reverse` | `Stage a a` | Give the items in reverse order. |
| `count` | `Stage a Int` | Give the number of items, as one item. |
| `each` | `[a] -> Stream Bytes a ()` | Emit a finite list as stream items. |
| `length` | `[a] -> Int` | Count list items. |

`sorted`, `sortedOn`, `reverse` and `count` wait for the end of their input, as
`table` does, so they never end on an endless source. `take` ends one:
after n items it stops, and so do the stages before it, as `head` stops
`yes`. `unique` streams, and keeps every value it has seen.

```text
ls "." | sortedOn .size | reverse | take 3 | map .name
```

The three largest entries here, by size.

An adapter or collector is a value. A bare `lines` shows `<stage>` and runs nothing.
Adapters compose with other adapters, pure functions and collectors.
A stream starts with a command, `each`, or an explicit value or computation.
Put the source before its adapters: `cat | lines | encodeLine | cat`.

```text
clean = lines | filter (x -> not (isEmpty x))
readClean = clean | collect
r <- printf "a\n\nb\n" | readClean
```

Both `clean` and `readClean` are reusable. Each run gets fresh line and collection state.
`=` names a plan and runs nothing. `:t` shows the effects of the full action.

## Lists and pure data

Lists are finite and homogeneous. Their elements can include records, functions and latent actions.

```text
xs = [{ok = true, size = 3}, {ok = false, size = 8}]
r <- each xs | filter .ok | .size | collect
r.items
length r.items
```

The results are `[3]` and `1`.
A pure source has no process status, so its `status` field is `()`.
A command returns its status, including nonzero exits and signals.
When two streams connect, the right stream supplies the return value.
An `each` source on the right therefore supplies `()` even if the left stream contains a command.
Adapters preserve that return value.

## Statuses and `pipefail`

`collect` also records every external's status, in pipeline order:

```text
yes | head -1 | lines | collect
{items = ["y"], status = exit 0, statuses = [signal 13, exit 0]}
```

`statuses` is `[]` when nothing external ran. The return value is still the
last external's status.

`pipefail` makes a command stream return the first failure instead:

```text
pipefail : Stream a b Status -> Stream a b Status

pipefail (sh -c "exit 3" | sh -c "exit 4" | cat)   -- exit 3
pipefail (yes | head -1)                           -- success
```

A stage killed by SIGPIPE before the last stage has not failed. A closed
reader is how a pipeline cancels its producers. SIGPIPE in the last stage is a
failure. `pipefail` survives later stages, so
`pipefail (a | b) | lines | collect` has the pipefail status.

Pure functions after a stream map its items, so `.size` needs no explicit `map`.
`map` is useful when a transform should be a reusable adapter value.
`filter` accepts a predicate with result `Bool`; it does not run computation-valued predicates.

String helpers include `contains needle`, `startsWith prefix`, `isEmpty` and `stringLength`.
`stringLength` counts characters as Haskell `Char` values, rather than bytes or grapheme clusters.
`encodeLine` explicitly encodes a string as UTF-8 and adds LF before an external stage.

## Framing and errors

`lines` splits on LF and removes a preceding CR for CRLF input.
It preserves blank lines, spaces, NUL and an unterminated final line.
A final LF does not add an extra empty item. An unterminated CR remains data.
UTF-8 characters and CRLF pairs can cross any input chunk boundary.
Framing retains fragments of the current line and assembles each completed line once.

Invalid or incomplete UTF-8 produces `lines: invalid or incomplete UTF-8`.
The runtime cancels workers, closes resources and reaps direct children before the REPL reports the error.
A failed `<-` keeps the old binding. Collection exposes no partial list.
Commands can already have run when a data error occurs.

Adapters use bounded buffers, plus the storage needed for the current line.
`collect` deliberately retains all output items. It needs a finite stream to finish and uses memory proportional to the result.
Without `lines`, collection retains raw `Bytes` chunks whose boundaries can vary between runs.
