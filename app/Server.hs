{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @lambdash --lsp@: a language server on stdin and stdout. It reports
-- problems and shows types on hover. Each open file is checked as a startup
-- file would run, but nothing runs. See "Lambdash.Check".
module Server (serve) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Lambdash.Check (Problem (..), checkText, hoverAt)
import Lambdash.Repl (Session (..), initial)
import System.FilePath (takeDirectory)

serve :: IO Int
serve = runServer ServerDefinition
  { parseConfig = const (const (Right ()))
  , onConfigChange = const (pure ())
  , defaultConfig = ()
  , configSection = "lambdash"
  , doInitialize = \env _ -> pure (Right env)
  , staticHandlers = const handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  -- Without this, the library tells the client to send no open or change
  -- notifications. The library keeps the text from incremental changes.
  , options = defaultOptions
      { optTextDocumentSync = Just (TextDocumentSyncOptions (Just True) (Just TextDocumentSyncKind_Incremental) Nothing Nothing Nothing) }
  }

-- The library keeps each open file's text; a change only rechecks it.
handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_Initialized (const (pure ()))
  , notificationHandler SMethod_TextDocumentDidOpen $ \(TNotificationMessage _ _ (DidOpenTextDocumentParams (TextDocumentItem uri _ _ _))) ->
      recheck (toNormalizedUri uri)
  , notificationHandler SMethod_TextDocumentDidChange $ \(TNotificationMessage _ _ (DidChangeTextDocumentParams (VersionedTextDocumentIdentifier uri _) _)) ->
      recheck (toNormalizedUri uri)
  , notificationHandler SMethod_TextDocumentDidSave (const (pure ()))
  -- VS Code sends its settings at start and on each change. The server has
  -- none, but without a handler the library shows the client an error.
  , notificationHandler SMethod_WorkspaceDidChangeConfiguration (const (pure ()))
  -- The lsp library always advertises semantic tokens, so a client asks.
  -- None yet: an empty answer, not an error.
  , requestHandler SMethod_TextDocumentSemanticTokensFull $ \_ respond -> respond (Right (InR Null))
  , requestHandler SMethod_TextDocumentHover $ \(TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) (Position line column) _)) respond -> do
      found <- getVirtualFile (toNormalizedUri uri)
      let text = maybe "" (Text.unpack . virtualFileText) found
          row = fromIntegral line + 1
      -- The protocol counts from 0 in UTF-16 units, and Check from 1 in
      -- characters.
      at <-liftIO (hoverAt (startFor (toNormalizedUri uri)) text row (fromUnits (lineOf text row) (fromIntegral column) + 1))
      respond (Right (maybe (InR Null) (InL . hover text) at))
  , notificationHandler SMethod_TextDocumentDidClose $ \(TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) ->
      publishDiagnostics maxProblems (toNormalizedUri uri) Nothing (partitionBySource [])
  ]

-- | Check the file as the editor has it now, and replace its problems.
recheck :: NormalizedUri -> LspM () ()
recheck uri = do
  found <- getVirtualFile uri
  case found of
    Nothing -> pure ()
    Just file -> do
      let text = Text.unpack (virtualFileText file)
      problems <- liftIO (checkText (startFor uri) text)
      publishDiagnostics maxProblems uri (Just (virtualFileVersion file)) (partitionBySource (map (diagnostic text) problems))

-- | The session a file starts in. A relative import starts at the file's
-- directory, as in a module; a file that is not on disk has none, so its
-- imports start at the server's directory.
startFor :: NormalizedUri -> Session
startFor uri = initial {sessionDirectory = takeDirectory <$> uriToFilePath (fromNormalizedUri uri)}

-- | The type as code, in a block that the editor highlights as λsh.
hover :: String -> (Int, Int, Int, String) -> Hover
hover text (line, from, to, shown) =
  Hover (InL (MarkupContent MarkupKind_Markdown (Text.pack ("```lambdash\n" <> shown <> "\n```"))))
    (Just (Range (Position (fromIntegral (line - 1)) (units from)) (Position (fromIntegral (line - 1)) (units to))))
 where
  units column = fromIntegral (toUnits (lineOf text line) (column - 1))

maxProblems :: Int
maxProblems = 100

-- | Lines and columns count from 0 in the protocol, and from 1 in a
-- 'Problem'. A problem without a column covers its lines.
diagnostic :: String -> Problem -> Diagnostic
diagnostic text problem =
  -- Range, severity, code, code description, source, message, tags,
  -- related information, data.
  Diagnostic (Range start end) (Just DiagnosticSeverity_Error) Nothing Nothing (Just "lambdash")
    (Text.pack (problemMessage problem)) Nothing Nothing Nothing
 where
  line n = fromIntegral (n - 1)
  (start, end) = case problemColumn problem of
    Just column ->
      let units n = fromIntegral (toUnits (lineOf text (problemLine problem)) n)
      in (Position (line (problemLine problem)) (units (column - 1)), Position (line (problemLine problem)) (units column))
    Nothing -> (Position (line (problemLine problem)) 0, Position (line (problemEndLine problem) + 1) 0)

-- | A line of the text, counting from 1, or nothing past the end.
lineOf :: String -> Int -> String
lineOf text n = concat (take 1 (drop (n - 1) (lines text)))

-- | The protocol counts a column in UTF-16 code units, and Check in
-- characters. A character beyond the Basic Multilingual Plane is two units.
-- These take and give a count from 0. A column past the end of the line
-- counts each missing character as one unit.
toUnits :: String -> Int -> Int
toUnits text chars = sum (map width (take chars text)) + max 0 (chars - length text)

fromUnits :: String -> Int -> Int
fromUnits text = go 0 text
 where
  go chars _ 0 = chars
  go chars (c : rest) units
    | units < width c = chars
    | otherwise = go (chars + 1) rest (units - width c)
  go chars [] units = chars + units

width :: Char -> Int
width c = if fromEnum c > 0xFFFF then 2 else 1
