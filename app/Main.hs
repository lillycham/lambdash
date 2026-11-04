module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Lambdash.Nix (withRootDirectory)
import Lambdash.Process (saveShellTerminal, setupTerminal)
import Lambdash.Repl (Session (..), Step (..), arrive, initial, needsMore, notices, runScript, sessionPrompt, startupFile, step)
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import Server (serve)
import System.Console.Haskeline
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import System.Directory (getCurrentDirectory)
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stderr, stdin, stdout)

main :: IO ()
main = do
  -- A shell must not die on the locale. Under LANG=C, GHC would encode text as
  -- ASCII and throw on the first non-ASCII character, so text is always UTF-8:
  -- the terminal, arguments, paths and the environment.
  --
  -- Note: haskeline still decodes keys by the locale, so under LANG=C a typed
  -- é arrives as replacement characters. It no longer crashes the shell.
  setLocaleEncoding utf8
  setForeignEncoding utf8
  setFileSystemEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  -- An editor talks to the server on stdin and stdout, so no terminal setup.
  lsp <- ("--lsp" `elem`) <$> getArgs
  if lsp then serve >>= exitWith . code else shell
 where
  code 0 = ExitSuccess
  code n = ExitFailure n

shell :: IO ()
shell = do
  -- Computations print straight to stdout, beside the output of haskeline.
  hSetBuffering stdout LineBuffering
  setupTerminal
  restoreTerminal <- saveShellTerminal
  withRootDirectory $ \roots -> do
    -- Starting in a directory is arriving in it.
    cwd <- getCurrentDirectory
    (arrived, messages) <- arrive cwd initial {sessionRoots = roots}
    mapM_ putStrLn messages
    -- The startup file runs before the first prompt. --no-init skips it.
    args <- getArgs
    start <- if "--no-init" `elem` args
      then pure arrived
      else do
        path <- startupFile
        (ready, reports) <- runScript path arrived
        restoreTerminal
        mapM_ putStrLn reports
        pure ready
    runInputT defaultSettings (loop restoreTerminal start)

-- | What the prompt read.
data Input
  = Line String
  | -- | Ctrl-C: drop what was typed, including an open block, and prompt
    -- again, as other shells do.
    Cancelled
  | End

-- | Read a line, and continuation lines while a @do@ block is open.
--
-- The shell ignores SIGINT, so that Ctrl-C reaches only the foreground job.
-- While reading, haskeline handles it instead, for the length of the read.
readInput :: String -> InputT IO Input
readInput prompt = handleInterrupt (pure Cancelled) $ withInterrupt $ do
  first <- getInputLine prompt
  maybe (pure End) (fmap Line . more) first

-- | End of input stops reading, and the parser reports the unclosed block.
more :: String -> InputT IO String
more input
  | needsMore input = do
      next <- getInputLine ".. "
      maybe (pure input) (\line -> more (input <> "\n" <> line)) next
  | otherwise = pure input

-- | All of the meaning is in 'step'. This only moves text.
loop :: IO () -> Session -> InputT IO ()
loop restoreTerminal before = do
  -- Background jobs report at the next prompt, as in other shells.
  (session, messages) <- liftIO (notices before)
  mapM_ outputStrLn messages
  cwd <- liftIO getCurrentDirectory
  input <- readInput (sessionPrompt cwd session)
  case input of
    End -> pure ()
    Cancelled -> loop restoreTerminal session
    Line line -> do
      result <- liftIO (step session line)
      -- Whatever the line did to the terminal, the shell owns it again, in
      -- its own modes, before printing anything.
      liftIO restoreTerminal
      case result of
        Quit -> pure ()
        Failed output -> do
          outputStrLn output
          loop restoreTerminal session
        Continue next output -> do
          unless (null output) (outputStrLn output)
          loop restoreTerminal next
