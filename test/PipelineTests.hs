module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, withAsync)
import Control.Exception hiding (handle)
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.List (isInfixOf)
import Data.Set qualified as Set
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Lambdash.Eval (typeEnv)
import Lambdash.Infer
import Lambdash.Parser
import Lambdash.Process (Status (..))
import Lambdash.Repl hiding (command)
import Lambdash.Stream
import Lambdash.Unify
import System.Directory
import System.Exit (exitFailure)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Process (ProcessStatus, getAnyProcessStatus, getProcessStatus)
import System.Posix.Types (ProcessID)
import System.Timeout (timeout)

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
typed source expected = case parseExprIn (BoundNames (Set.fromList ["pure", "add", "not", "byteLength", "encodeUtf8"])) source of
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
  [ ("effects: latent function needs no command", gives ["f = x -> missing-effects-xyz", "pure 1"] "" ["f : a -> Stream Bytes Bytes Status", "1"])
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
  , ("pipe: a generic stage keeps its inferred meaning", gives ["f = s -> s | (x -> 1)", "f (printf abc)"] "1\n" ["f : Stream a b c -> Stream a Int c", ""])
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
          action = void (runStages id id (BS.hPut stdout) [External "sh" ["-c", command, "test", path]])
      withAsync action $ \task -> awaitFile path >> cancel task
      done <- reaped path
      pure done)
  , ("cleanup: a failed worker kills and reaps the child", withPath $ \path -> do
      let command = "echo $$ > \"$1\"; printf ready; exec sleep 100"
      result <- try (runStages id id (void . evaluate)
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
      statuses <- mapM (\_ -> runStages id id (BS.hPut stdout)
        [External "sleep" ["100"], External path []]) [1 :: Int .. 3]
      children <- try (getAnyProcessStatus False False) :: IO (Either IOException (Maybe (ProcessID, ProcessStatus)))
      pure (statuses == replicate 3 (ExitedWith 127) && either isDoesNotExistError (const False) children))
  ]

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
  results <- mapM runTest tests
  putStrLn (show (length (filter id results)) <> "/" <> show (length results) <> " passed")
  unless (and results) exitFailure
