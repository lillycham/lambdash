{-# LANGUAGE LambdaCase #-}

-- | Job-control spike: can a Haskell shell under the threaded RTS run a
-- pipeline in its own process group, hand over the terminal, and survive
-- Ctrl-C, Ctrl-Z and fg? Not part of the shell proper.
module Main (main) where

import Control.Exception (IOException, finally, onException, try)
import Control.Monad (unless, void, when)
import Data.List (intercalate, (\\))
import Foreign hiding (void)
import Foreign.C
import System.Exit (ExitCode (..), die)
import System.IO
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Terminal
import System.Posix.Types

foreign import ccall safe "lsh_spawn"
  c_spawn :: CString -> Ptr CString -> Fd -> Fd -> ProcessGroupID -> Ptr ProcessID -> IO CInt

data Job = Job
  { jobGroup :: ProcessGroupID
  , jobLive :: [ProcessID]
  , jobCommand :: String
  , jobTty :: TerminalAttributes
  }

data Shell = Shell
  { shellGroup :: ProcessGroupID
  , shellTty :: TerminalAttributes
  }

data Outcome
  = Finished [ProcessStatus]
  | Suspended [ProcessID]

-- Spawn one stage. The child starts its own group when the group is 0.
spawn :: [String] -> Fd -> Fd -> ProcessGroupID -> IO ProcessID
spawn [] _ _ _ = ioError (userError "empty command")
spawn argv@(file : _) input output group =
  withMany withCString argv $ \cstrs ->
    withArray0 nullPtr cstrs $ \cargv ->
      withCString file $ \cfile ->
        alloca $ \pid -> do
          err <- c_spawn cfile cargv input output group pid
          unless (err == 0) $
            ioError (errnoToIOError "spawn" (Errno err) Nothing (Just file))
          peek pid

-- Spawn the stages left to right, all in the first stage's group.
spawnPipeline :: [[String]] -> IO [ProcessID]
spawnPipeline = go stdInput 0 []
 where
  go _ _ pids [] = pure (reverse pids)
  go input group pids [stage] = do
    pid <- spawn stage input stdOutput group `onException` abort pids `finally` closeInput input
    pure (reverse (pid : pids))
  go input group pids (stage : rest) = do
    (readEnd, writeEnd) <- createPipe
    setFdOption readEnd CloseOnExec True
    setFdOption writeEnd CloseOnExec True
    pid <-
      (spawn stage input writeEnd group `onException` (abort pids >> closeFd readEnd))
        `finally` (closeFd writeEnd >> closeInput input)
    go readEnd (if group == 0 then pid else group) (pid : pids) rest

  closeInput input = unless (input == stdInput) (closeFd input)
  abort pids = case reverse pids of
    [] -> pure ()
    leader : _ -> quietly (signalProcessGroup sigTERM leader)

-- macOS refuses kill and tcsetpgrp for a group that holds only zombies, and a
-- short command such as `true` can be one before the shell gets here.
quietly :: IO () -> IO ()
quietly = void . (try :: IO a -> IO (Either IOException a))

-- Wait until every stage has ended, or every live stage has stopped.
waitJob :: ProcessGroupID -> [ProcessID] -> IO Outcome
waitJob group = go [] []
 where
  go done stopped live
    | null live = pure (Finished (reverse done))
    | null (live \\ stopped) = pure (Suspended live)
    | otherwise =
        getGroupProcessStatus True True group >>= \case
          Nothing -> go done stopped live
          -- The child read the terminal before the shell handed it over.
          Just (_, Stopped sig)
            | sig == sigTTIN || sig == sigTTOU -> do
                quietly (signalProcessGroup sigCONT group)
                go done stopped live
          Just (pid, Stopped _) -> go done (pid : stopped) live
          Just (pid, status) -> go (status : done) stopped (filter (/= pid) live)

-- Give the job the terminal, wake it, wait, and take the terminal back.
-- The unconditional SIGCONT also clears the race with waitJob's first case.
foreground :: Shell -> ProcessGroupID -> [ProcessID] -> TerminalAttributes -> IO (Outcome, TerminalAttributes)
foreground shell group live tty = do
  quietly (setTerminalProcessGroupID stdInput group)
  setTerminalAttributes stdInput tty Immediately
  quietly (signalProcessGroup sigCONT group)
  outcome <- waitJob group live
  jobTty' <- getTerminalAttributes stdInput
  setTerminalProcessGroupID stdInput (shellGroup shell)
  setTerminalAttributes stdInput (shellTty shell) Immediately
  pure (outcome, jobTty')

settle :: [Job] -> Job -> (Outcome, TerminalAttributes) -> IO [Job]
settle jobs job = \case
  (Finished statuses, _) -> do
    putStrLn ("[" <> intercalate ", " (map describe statuses) <> "]")
    pure jobs
  (Suspended live, tty) -> do
    putStrLn ("[stopped] " <> jobCommand job)
    pure (job {jobLive = live, jobTty = tty} : jobs)

describe :: ProcessStatus -> String
describe = \case
  Exited ExitSuccess -> "exit 0"
  Exited (ExitFailure n) -> "exit " <> show n
  Terminated sig _ -> "signal " <> show sig
  Stopped sig -> "stopped " <> show sig

splitStages :: String -> [[String]]
splitStages = filter (not . null) . go [] . words
 where
  go stage [] = [reverse stage]
  go stage ("|" : rest) = reverse stage : go [] rest
  go stage (word : rest) = go (word : stage) rest

run :: Shell -> [Job] -> String -> IO [Job]
run shell jobs line =
  case splitStages line of
    [] -> pure jobs
    stages ->
      try (spawnPipeline stages) >>= \case
        Left err -> do
          hPutStrLn stderr ("spawn failed: " <> show (err :: IOException))
          pure jobs
        Right pids -> do
          let group = head pids
              job = Job group pids line (shellTty shell)
          foreground shell group pids (shellTty shell) >>= settle jobs job

loop :: Shell -> [Job] -> IO ()
loop shell jobs = do
  putStr "spike> "
  eof <- isEOF
  unless eof $
    getLine >>= \case
      "exit" -> pure ()
      "jobs" -> mapM_ (putStrLn . jobCommand) jobs >> loop shell jobs
      "fg" -> case jobs of
        [] -> putStrLn "no stopped jobs" >> loop shell jobs
        job : rest ->
          foreground shell (jobGroup job) (jobLive job) (jobTty job)
            >>= settle rest job
            >>= loop shell
      line -> run shell jobs line >>= loop shell

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  tty <- queryTerminal stdInput
  unless tty (die "spike-jobs needs a terminal")
  original <- getTerminalProcessGroupID stdInput
  mapM_
    (\sig -> installHandler sig Ignore Nothing)
    [sigINT, sigQUIT, sigTSTP, sigTTIN, sigTTOU]
  -- A session leader, as under expect, already leads its group.
  quietly (setProcessGroupIDOf 0 0)
  group <- getProcessGroupID
  setTerminalProcessGroupID stdInput group
  attrs <- getTerminalAttributes stdInput
  loop (Shell group attrs) [] `finally` when (original /= group) (setTerminalProcessGroupID stdInput original)
