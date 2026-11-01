# Pipelines

An external command has type `Stream {exec command} Bytes Bytes Status`.
The parameters mean effect row, input item, output item and return value.
Normal prompt output omits effects. `:t` shows them.
`Bytes` holds a finite byte chunk. It is separate from `String`.

```text
printf hello | cat
yes | head -1
printf hello | (chunk -> chunk) | cat
pure {size = 3} | .size | add 2
encodeUtf8 "hello" | cat
```

`|` connects the output of one stream to the input of the next stream.
A pure function after a stream maps each output item.
A pure function after `Comp a` maps its return value.
A pure function after a plain value applies to that value.
A value or computation before a stream supplies one input item.

A generic producer, such as `s` in `s -> s | byteLength`, has a stream type.
This choice occurs during inference, so later calls cannot change its meaning.
Use `pure value | stage` to supply one item from a generic value.

## Bytes and text

Adjacent external commands use an OS pipe.
The runtime does not decode their data or retain the whole output.
Pure stages receive byte chunks of at most 32 KiB at an external boundary.
Chunk boundaries can change between runs. They are not lines or text boundaries.

`encodeUtf8 :: String -> Bytes` makes an explicit UTF-8 byte value.
`byteLength :: Bytes -> Int` measures one finite byte value.
After an external, `byteLength` reports each chunk length, not the total length.
Use an external such as `wc -c` for a total until stream folds exist.

The prompt writes byte output unchanged and shows other output items one per line.
There is no implicit conversion from a record, integer or string into external input.
Incremental text decoders, line parsers, folds and output collection are future work.

## Effects and status

`=` names a stream without a spawn. A bare prompt line runs it.
`<-` runs it and binds its return value, while its output goes to the terminal.

```text
copy = printf hello | cat
copy
s <- sh -c "exit 7" | cat
s
```

The last external command supplies the pipeline status. In this example, `s` is `exit 0`.
A pure stage preserves that status. There is no `pipefail` option yet.
An upstream `SIGPIPE` after a downstream exit is normal cancellation.

Inference checks the whole expression before any stage runs.
The REPL preflight reads the outer action's inferred effect row.
It resolves external names through bindings and higher-order functions before any action runs.
Latent actions in records, functions and return values keep their own requirements.
The runtime also checks the concrete stream plan before the first spawn.
A runtime spawn failure returns status 127 and cancels the stages that did start.
The executable can still change after preflight; that race cannot be eliminated here.

Pure stages run concurrently with external stages and use bounded buffers.
A downstream close propagates upstream through OS pipes and pure stage queues.
An exception or caller cancellation closes resources, kills the job group and reaps direct children.
All external stages share one foreground process group, so terminal signals reach the whole job.
Ctrl-Z resumes the group immediately because the shell has no job table yet.

See [effect rows](effects.md) for row inference and current solver limits.
