module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Exception hiding (handle)
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.List (isInfixOf)
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Lambdash.Eval (Return (..), Value (..), runComp, typeEnv)
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Process (Status (..))
import Lambdash.Repl hiding (command)
import Lambdash.Overlay (emptyState)
import Lambdash.Runtime (Runtime, newRuntime)
import Lambdash.Stream
import Lambdash.Unify
import System.Directory
import System.Exit (exitFailure)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (ProcessStatus, getAnyProcessStatus, getProcessStatus)
import System.Posix.Types (ProcessID)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

-- | A session with no environments, for running plans directly.
runtime :: Runtime
runtime = unsafePerformIO (newRuntime emptyState)
{-# NOINLINE runtime #-}

-- Strict binary capture, with stdout restored even if an action fails.
capture :: IO a -> IO (BS.ByteString, a)
capture action = bracket acquire release $ \(_, path, _) -> do
  value <- action
  hFlush stdout
  bytes <- BS.readFile path
  pure (bytes, value)
 where
  acquire = do
    hFlush stdout
    saved <- hDuplicate stdout
    directory <- getTemporaryDirectory
    (path, handle) <- openBinaryTempFile directory "pipeline-output"
    hDuplicateTo handle stdout
    hClose handle
    pure (saved, path, ())
  release (saved, path, _) = do
    hFlush stdout `finally` hDuplicateTo saved stdout
    hClose saved
    removeFile path

linesIO :: [String] -> IO [String]
linesIO = go initial
 where
  go _ [] = pure []
  go session (line : rest) = step session line >>= \result -> case result of
    Quit -> pure ["<quit>"]
    Continue next text -> (text :) <$> go next rest

gives :: [String] -> String -> [String] -> IO Bool
gives input output results = do
  actual <- capture (linesIO input)
  pure (actual == (B8.pack output, results))

typed :: String -> String -> Bool
typed source expected = case parseExprIn (BoundNames (Map.keysSet typeEnv)) source of
  Left _ -> False
  Right expression -> fmap renderType (inferIn typeEnv expression) == Right expected

withPath :: (FilePath -> IO a) -> IO a
withPath action = bracket acquire (\path -> do exists <- doesFileExist path; if exists then removeFile path else pure ()) action
 where
  acquire = do
    directory <- getTemporaryDirectory
    (path, handle) <- openTempFile directory "pipeline-marker"
    hClose handle
    removeFile path
    pure path

-- A child writes its pid before the test interrupts the runner. ECHILD after
-- unwind proves that the runner reaped it, rather than only sending SIGKILL.
reaped :: FilePath -> IO Bool
reaped path = do
  pid <- read <$> readFile path :: IO ProcessID
  outcome <- try (getProcessStatus False False pid) :: IO (Either IOException (Maybe ProcessStatus))
  pure $ case outcome of
    Left err -> isDoesNotExistError err
    Right _ -> False

awaitFile :: FilePath -> IO ()
awaitFile path = do
  exists <- doesFileExist path
  if exists then do
    text <- readFile path
    if null text then threadDelay 5000 >> awaitFile path else pure ()
  else threadDelay 5000 >> awaitFile path

tests :: [(String, IO Bool)]
tests =
  [ ("lists: commas separate latent external commands", gives ["length [echo a, echo b]"] "" ["2"])
  , ("lists: parentheses preserve a comma inside a command", gives ["length [(echo a,b), (echo c)]"] "" ["2"])
  , ("records: commas separate command-valued fields", gives ["r = {a = echo hi, b = 2}", "r.b"] ""
      ["r : {a : Stream Bytes Bytes Status, b : Int}", "2"])
  , ("commands: ordinary argument commas remain literal", gives ["echo a,b"] "a,b\n" [""])
  , ("lists: literals and length", gives ["[1, 2, 3]", "length [1, 2, 3]"] "" ["[1, 2, 3]", "3"])
  , ("lists: nested and empty literals", gives ["[[], [1, 2]]", ":t []"] "" ["[[], [1, 2]]", "[a]"])
  , ("lists: empty bindings remain polymorphic", gives ["xs = []", "each xs | filter isEmpty | collect", "each xs | add 1 | collect"] ""
      ["xs : [a]", "{items = [], status = (), statuses = []}", "{items = [], status = (), statuses = []}"])
  , ("lists: heterogeneous items fail", do
      (_, results) <- capture (linesIO ["[\"a\", true]"])
      pure (any (isInfixOf "type mismatch") results))
  , ("lists: actions stay latent", gives ["[print 1, print 2]", "each [print 1] | collect"] ""
      ["[<computation>, <computation>]", "{items = [<computation>], status = (), statuses = []}"])
  , ("lines: collected external output", gives ["printf \"owl\\ncat\\n\" | lines | collect"] ""
      ["{items = [\"owl\", \"cat\"], status = exit 0, statuses = [exit 0]}"])
  , ("lines: a finite byte value can be a source", gives ["encodeUtf8 \"λ\\ncat\" | lines | collect"] ""
      ["{items = [" <> show "λ" <> ", \"cat\"], status = (), statuses = []}"])
  , ("lines: a computation can be a byte source", gives ["pure (encodeUtf8 \"owl\\n\") | lines | collect"] ""
      ["{items = [\"owl\"], status = (), statuses = []}"])
  , ("filter: string predicates select lines", gives ["printf \"owl\\ncat\\nbarn owl\\n\" | lines | filter (contains \"owl\") | collect"] ""
      ["{items = [\"owl\", \"barn owl\"], status = exit 0, statuses = [exit 0]}"])
  , ("filter: composed pure predicates skip empty items", gives ["each [\"owl\", \"\", \"cat\"] | filter (x -> not (isEmpty x)) | collect"] ""
      ["{items = [\"owl\", \"cat\"], status = (), statuses = []}"])
  , ("filter: Boolean data remains structured", gives ["each [true, false, true] | filter (x -> x) | collect"] ""
      ["{items = [true, true], status = (), statuses = []}"])
  , ("map: row accessors map structured records", gives ["each [{size = 1}, {size = 2}] | map .size | add 1 | collect"] ""
      ["{items = [2, 3], status = (), statuses = []}"])
  , ("stages: byte adapters compose and stay reusable", gives
      ["clean = lines | filter (x -> not (isEmpty x))", "readClean = clean | collect", "printf \"a\\n\\nb\\n\" | readClean", "printf \"c\\n\" | readClean"] ""
      ["clean : Stage Bytes String", "readClean : Collector Bytes String", "{items = [\"a\", \"b\"], status = exit 0, statuses = [exit 0]}", "{items = [\"c\"], status = exit 0, statuses = [exit 0]}"])
  , ("collect: items and status bind with <-", gives
      ["r <- printf \"a\\nb\\n\" | lines | collect", "r.items", "length r.items", "r.status"] ""
      ["r : {items : [String], status : Status, statuses : [Status]}", "[\"a\", \"b\"]", "2", "exit 0"])
  , ("collect: a nonzero status remains data", gives ["sh -c \"printf yes; exit 7\" | lines | filter (startsWith \"y\") | collect"] ""
      ["{items = [\"yes\"], status = exit 7, statuses = [exit 7]}"])
  , ("collect: a signal status remains data", gives ["sh -c \"printf yes; kill -9 $$\" | lines | collect"] ""
      ["{items = [\"yes\"], status = signal 9, statuses = [signal 9]}"])
  , ("collect: raw bytes are retained independent of chunking", gives
      ["r <- printf abc | collect", "each r.items", "r.status"] "abc"
      ["r : {items : [Bytes], status : Status, statuses : [Status]}", "", "exit 0"])
  , ("collect: naming it runs nothing", gives ["r = echo hi | lines | collect", ":t r"] ""
      ["r : Comp {items : [String], status : Status, statuses : [Status]}", "Comp {exec echo} {items : [String], status : Status, statuses : [Status]}"])
  , ("collect: every external's status, in order", gives ["sh -c \"exit 3\" | sh -c \"cat; exit 4\" | lines | collect"] ""
      ["{items = [], status = exit 4, statuses = [exit 3, exit 4]}"])
  , ("collect: a cancelled producer shows SIGPIPE", gives ["yes | head -1 | lines | collect"] ""
      ["{items = [\"y\"], status = exit 0, statuses = [signal 13, exit 0]}"])
  , ("pipefail: the type", pure $ typed "pipefail" "Stream a b Status -> Stream a b Status")
  , ("pipefail: the first failure", gives ["pipefail (sh -c \"exit 3\" | sh -c \"exit 4\" | cat)"] "" ["exit 3"])
  , ("pipefail: success is the last status", gives ["pipefail (echo hi | cat)"] "hi\n" [""])
  , ("pipefail: SIGPIPE upstream is cancellation, not failure", gives ["pipefail (yes | head -1)"] "y\n" [""])
  , ("pipefail: SIGPIPE in the last stage is a failure", gives ["pipefail (echo hi | sh -c \"kill -13 $$\")"] "" ["signal 13"])
  , ("pipefail: without it, the last status wins", gives ["sh -c \"exit 3\" | cat"] "" [""])
  , ("pipefail: it survives later stages", gives ["pipefail (sh -c \"exit 5\" | cat) | lines | collect"] ""
      ["{items = [], status = exit 5, statuses = [exit 5, exit 0]}"])
  , ("pipefail: it binds with <-", gives ["s <- pipefail (sh -c \"exit 6\" | cat)", "s"] "" ["s : Status", "exit 6"])
  , ("collect: a plain item collects once", gives ["42 | collect"] "" ["{items = [42], status = (), statuses = []}"])
  , ("collect: pure computation effects are retained", gives ["print 1 | collect"] "1\n" ["{items = [()], status = (), statuses = []}"])
  , ("collect: source failures cannot become a unit success", do
      result <- try (runComp runtime (VStream [SourceStage (const (ioError (userError "source failed"))), MapStage id] ReturnUnit)) :: IO (Either StreamError Value)
      pure $ case result of Left (StreamError _) -> True; _ -> False)
  , ("collect: a pure right stream retains its own return type", gives ["sh -c \"exit 7\" | each [1, 2] | collect"] ""
      ["{items = [1, 2], status = (), statuses = [exit 7]}"])
  , ("preflight: adapters preserve missing-command requirements", gives ["bad = missing-lines-command-xyz", "r <- bad | lines | filter isEmpty | collect"] ""
      ["bad : Stream Bytes Bytes Status", "command not found: missing-lines-command-xyz"])
  , ("preflight: predicates must be pure Booleans", do
      (output, results) <- capture (linesIO ["echo should-not-print | lines | filter (x -> pure true) | collect"])
      pure (BS.null output && any (isInfixOf "type mismatch") results))
  , ("types: list item constraints propagate", pure $ typed "x -> [x, add x 1]" "Int -> [Int]")
  , ("types: filters remain generic", pure $ typed "filter" "(a -> Bool) -> Stage a a")
  , ("types: collector item types follow maps", pure $ typed "each [{size = 1}] | .size | collect" "Num a => Comp {items : [a], status : (), statuses : [Status]}")
  , ("types: adapters require an explicit source", do
      (output, results) <- capture (linesIO ["filter (contains \"a\") | encodeLine | cat"])
      pure (BS.null output && any (isInfixOf "type mismatch") results))
  , ("lines: bare output prints unquoted strings", gives ["printf \"owl\\ncat\\n\" | lines"] "owl\ncat\n" [""])
  , ("cancel: demand crosses line framing and filters", gives ["yes | lines | filter (contains \"y\") | encodeLine | head -3"] "y\ny\ny\n" [""])
  , ("decode: invalid UTF-8 leaves a previous binding intact", gives
      ["r = 42", "r <- printf \"\\\\377\\n\" | lines | collect", "r"] ""
      ["r : Int", "lines: invalid or incomplete UTF-8", "42"])
  , ("decode: data errors do not end the session", gives ["printf \"\\\\302\" | lines | collect", "add 1 2"] ""
      ["lines: invalid or incomplete UTF-8", "3"])
  , ("decode: a worker error kills and reaps its producer", withPath $ \path -> do
      let script = "echo $$ > \"$1\"; printf '\\377\\n'; exec sleep 100"
          source = "sh -c " <> show script <> " test " <> show path <> " | lines | collect"
      (output, results) <- capture (linesIO [source])
      done <- reaped path
      pure (BS.null output && results == ["lines: invalid or incomplete UTF-8"] && done))
  , ("constraints: preflight proves a closed outer row", do
      let row = unionEffects (effect (Exec "cat")) (effectVar (Var 0))
          qualified = qualify [EffEqual row (effect (Exec "cat"))] (TStream row TBytes TBytes TStatus)
          session = initial {sessionTypes = (Map.singleton "job" (Forall [Var 0] qualified)), sessionValues = (Map.singleton "job" (VStream [External "cat" ["/dev/null"]] ReturnStatus))}
      (output, result) <- capture (step session "job")
      pure $ BS.null output && case result of Continue _ "" -> True; _ -> False)
  , ("constraints: proved requirements still get preflight", do
      let missing = effect (Exec "missing-constraint-command-xyz")
          row = unionEffects missing (effectVar (Var 0))
          qualified = qualify [EffEqual row missing] (TStream row TBytes TBytes TStatus)
          session = initial {sessionTypes = (Map.singleton "job" (Forall [Var 0] qualified)), sessionValues = (Map.singleton "job" (VStream [External "missing-constraint-command-xyz" []] ReturnStatus))}
      (output, result) <- capture (step session "job")
      pure $ BS.null output && case result of Continue _ "command not found: missing-constraint-command-xyz" -> True; _ -> False)
  , ("constraints: a binding retains and resolves its bound", gives
      ["bounded = " <> bounded, "bounded (cat /dev/null)"] "" [boundType, ""])
  , ("constraints: an invalid instance runs no command", gives
      ["bounded = " <> bounded, "bounded (printf side-effect)", "bounded (cat /dev/null)"] ""
      [boundType, "effect mismatch: {exec cat, exec printf} and {exec cat}", ""])
  , ("constraints: a run result keeps its qualification", gives
      ["bounded = " <> bounded, "f <- pure bounded", "f (printf side-effect)"] ""
      [boundType, "f : Stream Bytes Bytes a -> Stream Bytes Bytes Status", "effect mismatch: {exec cat, exec printf} and {exec cat}"])
  , ("constraints: record selection keeps a function's qualification", gives
      ["bounded = " <> bounded, "r = {f = bounded, n = 1}", "r.f (printf side-effect)", "r.n"] ""
      [boundType, "r : {f : Stream Bytes Bytes a -> Stream Bytes Bytes Status, n : Int}", "effect mismatch: {exec cat, exec printf} and {exec cat}", "1"])
  , ("constraints: unused locals still constrain enclosing inputs", gives
      ["local = s -> let unused = ((" <> bounded <> ") s) in pure 1", "local (printf side-effect)"] ""
      ["local : Num a => Stream Bytes Bytes b -> Comp a", "effect mismatch: {exec cat, exec printf} and {exec cat}"])
  , ("constraints: type query shows the deferred equality", gives
      [":t " <> bounded] ""
      ["({exec cat | a} = {exec cat}) => Stream { | a} Bytes Bytes b -> Stream {exec cat | a} Bytes Bytes Status"])
  , ("constraints: pure record pipeline type query is unchanged", gives
      ["pure {size = 3} | .size | add 2", ":t pure {size = 3} | .size | add 2"] "" ["5", "Comp Int"])
  , ("effects: latent function needs no command", gives ["f = x -> missing-effects-xyz", "pure 1"] "" ["f : a -> Stream Bytes Bytes Status", "1"])
  , ("effects: higher-order missing command gets a preflight error", gives ["f = x -> missing-effects-xyz", "(g x -> g x) f 1"] "" ["f : a -> Stream Bytes Bytes Status", "command not found: missing-effects-xyz"])
  , ("effects: latent record field needs no command", gives ["pure {job = missing-effects-xyz}"] "" ["{job = <stream>}"])
  , ("effects: selecting a command checks its row", gives ["r = {job = missing-effects-xyz}", "r.job"] "" ["r : {job : Stream Bytes Bytes Status}", "command not found: missing-effects-xyz"])
  , ("effects: a returned stream stays latent", gives ["c <- pure (missing-effects-xyz)", "c"] "" ["c : Stream Bytes Bytes Status", "command not found: missing-effects-xyz"])
  , ("effects: preflight precedes source computation effects", gives ["bad = missing-effects-xyz", "(print \"started\" | (x -> encodeUtf8 \"data\")) | bad"] "" ["bad : Stream Bytes Bytes Status", "command not found: missing-effects-xyz"])
  , ("effects: independent input rows instantiate separately", gives ["join = a b -> (a | cat) | (b | cat)", "join (printf abc) (head -c 3)"] "abc" ["join : Stream a Bytes b -> Stream Bytes Bytes c -> Stream a Bytes Status", ""])
  , ("effects: distinct comp effects instantiate separately", gives ["id = x -> x", "a = id (print 1)", "b = id (pure 2)", "a", "b"] "1\n" ["id : a -> a", "a : Comp ()", "b : Comp Int", "", "2"])
  , ("parser: left associative and application binds tighter", pure $
      parseExpr "f x | g y | h" == Right (EPipe (EPipe (EApp (EVar "f") (EVar "x")) (EApp (EVar "g") (EVar "y"))) (EVar "h")))
  , ("parser: a quoted pipe stays in its word", pure $
      parseExprIn (BoundNames Set.empty) "printf \"a|b\"|cat" == Right (EPipe (ECmd "printf" [AWord "a|b"]) (ECmd "cat" [])))
  , ("parser: incomplete pipes fail", pure $ all (either (const True) (const False) . parseExpr) ["x |", "| x", "x || y"])
  , ("type: an external separates bytes and status", pure $ typed "echo hi" "Stream Bytes Bytes Status")
  , ("type: external stages connect", pure $ typed "echo hi | cat" "Stream Bytes Bytes Status")
  , ("type: pure stages map output items", pure $ typed "echo hi | byteLength" "Stream Bytes Int Status")
  , ("type: pure computation output maps", pure $ typed "pure {size = 3} | .size | add 2" "Comp Int")
  , ("type: a generic producer is a stream", pure $ typed "s -> s | byteLength" "Stream a Bytes b -> Stream a Int b")
  , ("type: a pipe cannot turn Bytes into Int implicitly", pure $ not (typed "echo hi | add 1" "Stream Bytes Int Status"))
  , ("types: all stream positions substitute", pure $
      apply (singleton (Var 0) TInt) ((TStream noEffects) (TVar (Var 0)) ((TComp noEffects) (TVar (Var 0))) (TVar (Var 0))) == (TStream noEffects) TInt ((TComp noEffects) TInt) TInt)
  , ("types: all stream positions have free variables", pure $
      ftv ((TStream noEffects) (TVar (Var 0)) (TVar (Var 1)) (TVar (Var 2))) == Set.fromList [Var 0, Var 1, Var 2])
  , ("types: occurs checks the stream return value", pure $ occurs (Var 0) ((TStream noEffects) TBytes TBytes (TVar (Var 0))))
  , ("pipe: bytes flow through two externals", gives ["printf abc | cat"] "abc" [""])
  , ("pipe: Bytes source feeds an external", gives ["encodeUtf8 \"hello\" | cat"] "hello" [""])
  , ("pipe: computation source feeds an external", gives ["pure (encodeUtf8 \"hello\") | cat"] "hello" [""])
  , ("pipe: a pure value applies", gives ["1 | add 2"] "" ["3"])
  , ("pipe: structured computation maps", gives ["pure {size = 3} | .size | add 2"] "" ["5"])
  , ("pipe: a pure identity preserves bytes", gives ["printf abc | (x -> x) | cat"] "abc" [""])
  , ("pipe: pure stages change item types", gives ["printf abc | byteLength | add 2"] "5\n" [""])
  , ("pipe: a generic stage keeps its inferred meaning", gives ["f = s -> s | (x -> 1)", "f (printf abc)"] "1\n" ["f : Num a => Stream b c d -> Stream b a d", ""])
  , ("pipe: named stages stay reusable", gives ["source = printf hi", "copy = x -> x", "p = source | copy | cat", "p", "p"] "hihi"
      ["source : Stream Bytes Bytes Status", "copy : a -> a", "p : Stream Bytes Bytes Status", "", ""])
  , ("pipe: a type query runs nothing", gives [":t printf abc | byteLength"] "" ["Stream {exec printf} Bytes Int Status"])
  , ("pipe: a run binds the final status", gives ["s <- printf abc | sh -c \"cat; exit 7\"", "s"] "abc" ["s : Status", "exit 7"])
  , ("pipe: the last stage controls status", gives ["sh -c \"exit 9\" | cat"] "" [""])
  , ("pipe: a pure stage preserves status", gives ["sh -c \"printf abc; exit 9\" | byteLength"] "3\n" ["exit 9"])
  , ("cancel: yes terminates with a direct OS pipe", gives ["yes | head -1"] "y\n" [""])
  , ("cancel: demand crosses a pure stage", gives ["yes | (x -> x) | head -1"] "y\n" [""])
  , ("cancel: demand crosses bounded pure queues", gives ["yes | (x -> x) | (x -> x) | head -1"] "y\n" [""])
  , ("preflight: a missing downstream command runs nothing", gives ["echo side-effect | nope-pipeline-xyz"] "" ["command not found: nope-pipeline-xyz"])
  , ("preflight: bound commands also resolve before spawn", gives ["bad = nope-pipeline-xyz", "echo side-effect | bad"] "" ["bad : Stream Bytes Bytes Status", "command not found: nope-pipeline-xyz"])
  , ("preflight: type failure prevents a file write", withPath $ \path -> do
      (_, results) <- capture (linesIO ["sh -c \"touch " <> path <> "\" | add 1"])
      exists <- doesFileExist path
      pure (not exists && any (isInfixOf "type mismatch") results))
  , ("bytes: binary data survives pure stages", do
      (output, _) <- capture (linesIO ["printf \"\\\\377\\\\000\\\\200\" | (x -> x) | cat"])
      pure (output == BS.pack [255, 0, 128]))
  , ("bytes: UTF-8 encoding is explicit", gives ["encodeUtf8 \"λ\" | byteLength"] "" ["2"])
  , ("stream: a large output is drained concurrently", do
      (output, _) <- capture (linesIO ["head -c 200000 /dev/zero | (x -> x) | cat"])
      pure (BS.length output == 200000 && BS.all (== 0) output))
  , ("cleanup: cancellation kills and reaps the child", withPath $ \path -> do
      let command = "echo $$ > \"$1\"; exec sleep 100"
          action = void (runStages runtime id id (BS.hPut stdout) [External "sh" ["-c", command, "test", path]])
      withAsync action $ \task -> awaitFile path >> cancel task
      done <- reaped path
      pure done)
  , ("cleanup: a failed worker kills and reaps the child", withPath $ \path -> do
      let command = "echo $$ > \"$1\"; printf ready; exec sleep 100"
      result <- try (runStages runtime id id (void . evaluate)
        [External "sh" ["-c", command, "test", path], MapStage (\_ -> error "worker failed"), MapStage BS.copy]) :: IO (Either SomeException Status)
      done <- reaped path
      pure (either (const True) (const False) result && done))
  , ("preflight: a non-executable path runs nothing", withPath $ \path -> do
      writeFile path "not executable"
      gives ["echo side-effect | " <> path] "" ["command not found: " <> path])
  , ("cleanup: a partial spawn failure reaps earlier children", withPath $ \path -> do
      -- The file passes executable preflight but its interpreter cannot start.
      writeFile path "#!/no-such-pipeline-interpreter\n"
      permissions <- getPermissions path
      setPermissions path permissions {executable = True}
      outcomes <- mapM (\_ -> try (runStages runtime id id (BS.hPut stdout)
        [External "sleep" ["100"], External path []]) :: IO (Either StreamError Status)) [1 :: Int .. 3]
      children <- try (getAnyProcessStatus False False) :: IO (Either IOException (Maybe (ProcessID, ProcessStatus)))
      pure (all (either (const True) (const False)) outcomes && either isDoesNotExistError (const False) children))
  ]
 where
  bounded = "s -> ((f -> {a = f (s | cat), b = f (cat)}) (x -> x)).a"
  boundType = "bounded : Stream Bytes Bytes a -> Stream Bytes Bytes Status"

runTest :: (String, IO Bool) -> IO Bool
runTest (name, action) = do
  result <- try (timeout 5000000 action) :: IO (Either SomeException (Maybe Bool))
  let passed = resultIsTrue result
  putStrLn ((if passed then "pass  " else "FAIL  ") <> name <> detail result)
  pure passed
 where
  resultIsTrue (Right (Just True)) = True
  resultIsTrue _ = False
  detail (Left err) = ": " <> displayException err
  detail (Right Nothing) = " (timeout)"
  detail _ = ""

main :: IO ()
main = do
  -- Unit tests must not touch the terminal they were started from: with a
  -- tty on stdin, jobs hand the terminal over and back, which can stop the
  -- whole test run in an interactive shell. With stdin on /dev/null that code
  -- is skipped, and a program that reads stdin sees end of input.
  devNull <- openFile "/dev/null" ReadMode
  hDuplicateTo devNull stdin
  results <- mapM runTest tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
