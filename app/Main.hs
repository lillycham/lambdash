module Main (main) where

import Control.Monad (unless, void)
import Lambdash.Nix (withRootDirectory)
import Lambdash.Process (saveShellTerminal, setupTerminal)
import Lambdash.Repl (Session (..), Step (..), arrive, initial, needsMore, notices, runScript, sessionPrompt, startupFile, step)
import System.Environment (getArgs, lookupEnv)
import Lambdash.Complete (Candidate (..), completeName, isNameChar)
import System.Console.Isocline (CompletionEnv, completeFileName, completeWord, Completion (..), enableAutoTab, enableMultiline, readlineExMaybe, setHistory)
import Foreign.C.String (CString, withCString)
import GHC.IO.Encoding (setFileSystemEncoding, setForeignEncoding, setLocaleEncoding, utf8)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, hSetEncoding, stderr, stdin, stdout)

main :: IO ()
main = do
  -- A shell must not die on the locale. Under LANG=C, GHC would encode text as
  -- ASCII and throw on the first non-ASCII character, so text is always UTF-8:
  -- the terminal, arguments, paths and the environment.
  setLocaleEncoding utf8
  setForeignEncoding utf8
  setFileSystemEncoding utf8
  mapM_ (`hSetEncoding` utf8) [stdin, stdout, stderr]
  -- Computations print straight to stdout, between reads of the editor.
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
    history <- historyFile
    setHistory history 1000
    -- The prompt is the session's, whole: no marker after it.
    withCString "" (\none -> setPromptMarker none none)
    -- One line at a time: a block continues with ".. ", not inside the
    -- editor. The patch then reads a linefeed as Enter, which is what a
    -- key typed while a command ran arrives as.
    void (enableMultiline False)
    -- Tab with one completion takes it at once.
    void (enableAutoTab True)
    loop restoreTerminal start

-- | The binding's setPromptMarker passes "" as NULL, which means the
-- default marker, "> ". The C function takes "" as no marker.
foreign import ccall unsafe "ic_set_prompt_marker"
  setPromptMarker :: CString -> CString -> IO ()

-- | Where history goes: @$XDG_STATE_HOME/lambdash/history@, or
-- @~/.local/state/lambdash/history@, beside the approved flakes.
historyFile :: IO FilePath
historyFile = do
  state <- lookupEnv "XDG_STATE_HOME"
  home <- getHomeDirectory
  let base = case state of
        Just value | not (null value) -> value
        _ -> home </> ".local/state"
  createDirectoryIfMissing True (base </> "lambdash")
  pure (base </> "lambdash" </> "history")

-- | What the prompt read.
data Input
  = Line String
  | -- | Ctrl-C: drop what was typed, including an open block, and prompt
    -- again, as other shells do.
    Cancelled
  | End

-- | Read a line, and continuation lines while a @do@ block is open.
--
-- The editor reads the terminal raw, so Ctrl-C is a key while it reads, not
-- a signal. Patched isocline gives it as "\x03"; no line is end of input.
-- End of input in a block stops reading, and the parser reports the
-- unclosed block.
readInput :: Session -> String -> IO Input
readInput session prompt = readline (escape prompt) >>= \first -> case first of
  Nothing -> pure End
  Just line -> more line
 where
  more "\x03" = pure Cancelled
  more input
    | needsMore input = readline ".. " >>= \next -> case next of
        Nothing -> pure (Line input)
        Just "\x03" -> pure Cancelled
        Just line -> more (input <> "\n" <> line)
    | otherwise = pure (Line input)
  readline text = readlineExMaybe text (Just (complete session)) Nothing

-- | The prompt as plain text: the editor reads @[@ as the start of a style,
-- and an overlay's prompt has one.
escape :: String -> String
escape = concatMap (\c -> if c `elem` ("[\\" :: String) then ['\\', c] else [c])

-- | All of the meaning is in 'step'. This only moves text.
loop :: IO () -> Session -> IO ()
loop restoreTerminal before = do
  -- Background jobs report at the next prompt, as in other shells.
  (session, messages) <- notices before
  mapM_ putStrLn messages
  cwd <- getCurrentDirectory
  input <- readInput session (sessionPrompt cwd session)
  case input of
    End -> pure ()
    Cancelled -> loop restoreTerminal session
    Line line -> do
      result <- step session line
      -- Whatever the line did to the terminal, the shell owns it again, in
      -- its own modes, before printing anything.
      restoreTerminal
      case result of
        Quit -> pure ()
        Failed output -> do
          putStrLn output
          loop restoreTerminal session
        Continue next output -> do
          unless (null output) (putStrLn output)
          loop restoreTerminal next

-- | Tab: the session's names that start with the word before the cursor,
-- each with its type in the menu, then file names. Paths and the arguments
-- of a command are often both, so both show.
complete :: Session -> CompletionEnv -> String -> IO ()
complete session env input = do
  completeWord env input (Just isNameChar) $ \word ->
    [Completion (candidateText c) (candidateText c) (candidateHelp c) | c <- completeName session input word]
  completeFileName env input (Just '/') ["."] []
