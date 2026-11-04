-- | Terse built-in help: an overview, topics, and a line for each builtin.
-- Types are not written here: the REPL takes them from the session, so
-- @:help@ stays true after a name is rebound.
module Lambdash.Help
  ( overview
  , topic
  , topicNames
  , describe
  ) where

overview :: String
overview = unlines'
  [ "λsh: a typed, functional shell.  :help NAME or :help TOPIC for more."
  , ""
  , "prompt  :t e   type          :core e  desugared      :elab e  with dictionaries"
  , "        :env [VAR]  layers   :help [x]  this         :q  quit"
  , "shell   jobs  fg [n]  bg [n]  allow  deny            Ctrl-Z stops, Ctrl-C cancels"
  , "lines   x = e  bind          x <- e  run, then bind  f : t  signature"
  , "        let rec f = x -> … and g = y -> …  recursive functions"
  , "        class …  instance …  data T = A | B t  infixl 6 <+>  fixity"
  , "exprs   x -> e   f x   a | f | collect   do a; b end   with o do … end   e &"
  , "        if c then a else b   case e of p -> a; _ -> b end"
  , "        let x = e in b   let rec f = x -> … in b"
  , "        {a = 1}  r.a  .a   [1, 2]   \"s\"   (e : t)   (+ 1)   (.size > 1)"
  , "        git log --oneline   ls src   nixpkgs#jq   -- comment"
  , ""
  , "topics  " <> unwords topicNames
  ]

topicNames :: [String]
topicNames = map fst topics

topic :: String -> Maybe String
topic name = lookup name topics

topics :: [(String, String)]
topics =
  [ ("syntax", unlines'
      [ "A line is an expression, a binding (x = e, x <- e), a signature (f : t),"
      , "a declaration (class, instance, data) or a fixity line."
      , "An unbound name at the head is an external command: its words are"
      , "strings, $x is a variable. A bound name is a function."
      , "do a; b; c end runs in order; x <- e and x = e bind for what follows."
      , "if c then a else b: two computations join their effect rows."
      , "An open do asks for more lines; Ctrl-C drops it.   docs/grammar.md" ])
  , ("operators", unlines'
      [ "Loosest first: >>> <<< (r1)  || (r2)  && (r3)  == != < <= > >= (4)"
      , "<> (r5)  + - (l6)  * / % (l7)  others (l9). All bind tighter than |."
      , "x-1 and x - 1 subtract; f -1 passes -1. (- 1) is a section."
      , "(+), (+ 1), (1 +), (.size > 1). Define: (<+>) = a b -> …"
      , "infixl 6 <+> sets a fixity for the session.   docs/grammar.md" ])
  , ("pipelines", unlines'
      [ "a | b connects bytes between externals, and values between stages."
      , "lines splits bytes; map f, filter p, .field transform; collect ends:"
      , "  {items, status, statuses}.  pipefail s fails on the first failed stage."
      , "each [..] streams a list.   docs/pipelines.md, docs/streams.md" ])
  , ("jobs", unlines'
      [ "e & starts a job and gives a handle: j <- sleep 9 &.  wait j, fg j."
      , "Ctrl-Z stops the line; jobs lists; fg [n] resumes; bg [n] continues"
      , "without the terminal. A job's end is reported at the next prompt."
      , "   docs/jobs.md" ])
  , ("environments", unlines'
      [ "An overlay is a named list of patches: overlay \"o\" [set \"A\" \"1\"]."
      , "use o activates it, unuse \"o\" removes it, with o do … end scopes it."
      , "useLocal o: only in this directory and below. a <> b is one overlay,"
      , "b's patches last. :env lists layers; :env VAR shows who touched it."
      , "   docs/environments.md" ])
  , ("nix", unlines'
      [ "pkg \"nixpkgs#jq\" and dev \".#\" resolve to overlays. A bare reference"
      , "is sugar: use nixpkgs#jq, with .#bench do … end. A directory's flake"
      , "activates on arrival once you allow it; deny withdraws.   docs/nix.md" ])
  , ("modules", unlines'
      [ "pad = import \"lib/pad.lsh\" brings its bindings in as pad.padRight;"
      , "import \"lib/pad.lsh\" brings them in as they are. A module holds only"
      , "definitions, so an import runs nothing. Data, classes, instances and"
      , "operators are global. Paths start at the importing file.   docs/modules.md" ])
  , ("classes", unlines'
      [ "class Pretty a where pretty : a -> String"
      , "instance Pretty Int where pretty = n -> show n"
      , "class Eq a => H a where …  (superclass)   instance Show a => Show [a] …"
      , "where do m : t; m = default; … end for several members."
      , ":help CLASS lists its methods and instances.   docs/classes.md" ])
  , ("types", unlines'
      [ "a -> b   [a]   {name : String | r}   Comp a   Stream i o r   Job a"
      , "Rows as :t prints them: Comp {exec git, console | e} a. A closed row is"
      , "a ceiling. (e : t) narrows; f : Eq a => a -> Bool before f = … is rigid."
      , "   docs/types.md, docs/effects.md" ])
  , ("data", unlines'
      [ "data Maybe a = Nothing | Just a declares a type; Just is a function."
      , "case m of Just x -> x; Nothing -> 0 end: the first arm that matches."
      , "Patterns: _  x  Just p  {name = p, size}  0  \"s\"  true  (). Every value must be covered."
      , "let rec f = x -> … and g = y -> … binds recursive functions."
      , "   docs/data.md" ])
  , ("startup", unlines'
      [ "~/.config/lambdash/init.lsh ($XDG_CONFIG_HOME) runs before the first"
      , "prompt, line by line. Errors name their line.  --no-init skips it."
      , "   docs/startup.md" ])
  ]

-- | One line about a builtin, by name.
describe :: String -> Maybe String
describe name = lookup name builtins

builtins :: [(String, String)]
builtins =
  [ ("print", "print a value; a String without quotes")
  , ("pure", "a computation that does nothing and gives its argument")
  , ("andThen", "run a computation, then the one its result chooses")
  , ("collect", "end a pipeline: {items, status, statuses}")
  , ("map", "a stage that applies a function to each item")
  , ("filter", "a stage that keeps the items a predicate accepts")
  , ("lines", "a stage from bytes to lines, as Strings")
  , ("each", "a stream of a list's items")
  , ("pipefail", "a stream whose status is its first failed stage's")
  , ("length", "the number of items in a list")
  , ("sort", "sort a list, stably")
  , ("sortOn", "sort a list by a key, stably")
  , ("contains", "contains needle s: whether s contains needle")
  , ("startsWith", "startsWith prefix s")
  , ("isEmpty", "whether a String is empty")
  , ("stringLength", "the number of characters in a String")
  , ("byteLength", "the number of bytes")
  , ("encodeUtf8", "a String as UTF-8 bytes")
  , ("encodeLine", "a String and a newline, as UTF-8 bytes")
  , ("not", "logical not")
  , ("add", "add two Ints (+ works at any Num)")
  , ("show", "a value as text, through its Show instance")
  , ("display", "a value for a person, through its Display instance: a String without quotes")
  , ("fields", "a record's fields as {label, value} records, each value through Display")
  , ("table", "a stage that reads every record, then writes them as a table with fitted columns")
  , ("eq", "equality; also ==")
  , ("ne", "inequality; also !=")
  , ("lt", "less than; also <")
  , ("le", "at most; also <=")
  , ("gt", "greater than; also >")
  , ("ge", "at least; also >=")
  , ("min", "the lesser")
  , ("max", "the greater")
  , ("negate", "minus a number")
  , ("div", "integer division; also /")
  , ("mod", "remainder; also %")
  , ("cd", "change directory; no argument is ~, - is the last one")
  , ("ls", "a stream of directory entries: {name, type, size, ext}")
  , ("overlay", "overlay name patches: a named environment layer")
  , ("set", "set NAME value: a patch")
  , ("unset", "unset NAME: a patch")
  , ("prepend", "prepend NAME value: value:old, as for PATH")
  , ("append", "append NAME value: old:value")
  , ("prependWith", "prependWith sep NAME value: value<sep>old")
  , ("appendWith", "appendWith sep NAME value: old<sep>value")
  , ("use", "activate an overlay for the session; use REF resolves with pkg")
  , ("useLocal", "activate an overlay in this directory and below")
  , ("unuse", "remove an active overlay by name")
  , ("environments", "the names of the active overlays")
  , ("pkg", "resolve a flake reference's package to an overlay")
  , ("dev", "resolve a flake's devShell to an overlay")
  , ("wait", "wait for a job and give its result")
  , ("fg", "give a job the terminal and wait for it")
  , ("&&", "and, lazy on the right")
  , ("||", "or, lazy on the right")
  , (">>>", "f >>> g is x -> g (f x)")
  , ("<<<", "g <<< f is x -> g (f x)")
  , ("<>", "combine: strings, lists, bytes, overlays (later patches win)")
  , ("+", "add"), ("-", "subtract"), ("*", "multiply")
  , ("/", "integer division"), ("%", "remainder")
  , ("==", "equal"), ("!=", "not equal"), ("<", "less than")
  , ("<=", "at most"), (">", "greater than"), (">=", "at least")
  ]

-- | Lines joined without a trailing newline.
unlines' :: [String] -> String
unlines' = foldr1 (\a b -> a <> "\n" <> b)
