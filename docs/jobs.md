# Jobs

Ctrl-Z stops the foreground job and returns to the prompt. The job waits in
the table until `fg` resumes it.

```text
λ> cat
^Z
[1] stopped  cat
λ> jobs
[1] stopped  cat
λ> fg
```

| Command | |
| --- | --- |
| `jobs` | list stopped jobs |
| `fg` | resume the most recent |
| `fg 2`, `fg %2` | resume job 2 |

`jobs` and `fg` are commands only when nothing binds them, like an external
command. They act on the shell, so they are not values.

## What a job is

A job is a whole line, not only its processes. Each line runs on its own
thread. When every live process of the job has stopped, that thread does the
following:

1. It saves the job's terminal modes and gives the terminal back to the shell.
2. It parks, holding its processes, pipes and pure-stage workers as they are.
3. On `fg`, it restores the job's modes, gives the terminal to the job's
   process group and sends `SIGCONT`.

So a pipeline with pure stages stops and resumes as one, and a raw-mode
program gets its own modes back.

A process that stops with `SIGTTIN` or `SIGTTOU` before the job owns the
terminal is continued at once, as in the spike.

## Results arrive when the job finishes

- A stopped `x <- cmd` binds `x` when `fg` finishes it, in the session as it
  is then.
- A stopped line that changes the environments, such as
  `andThen (pkg "x") use`, commits its change when it finishes. That change
  was made from the environments when the line started. If they have changed
  since, the job's change is dropped with a warning rather than undoing the
  later one.

## Leaving

`:q` with stopped jobs warns once. A second `:q` quits. The kernel then sends
`SIGHUP` and `SIGCONT` to each stopped job, because its process group is
orphaned.

## Not yet

- `bg`. A background job needs completion notices at the prompt.
- `pipefail`. A pipeline's status is its last external's status. How the
  other statuses should appear is an open question in the plan.
