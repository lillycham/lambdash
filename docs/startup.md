# The startup file

Before the first prompt, λsh runs `$XDG_CONFIG_HOME/lambdash/init.lsh`, or
`~/.config/lambdash/init.lsh` without `XDG_CONFIG_HOME`. A missing file is
fine. `lambdash --no-init` skips it.

```text
-- ~/.config/lambdash/init.lsh
infixl 6 <+>
(<+>) = a b -> a * 10 + b

class Pretty a where pretty : a -> String
instance Pretty Int where pretty = n -> "#" <> show n

greet = name -> do
  print ("hello, " <> name);
  pure ()
end
```

- Each line runs as if typed at the prompt, in order, after the directory's
  flake (if allowed) activates. A `do` block may span lines.
- Bindings, fixity lines and declarations say nothing. Output, and the value
  of a bare expression, show as at the prompt.
- A line that fails is reported with its line number, and the lines after it
  still run:

  ```text
  /home/me/.config/lambdash/init.lsh:2: type mismatch: Int and Bool
  ```

- `:q` ends the file, not the shell.
