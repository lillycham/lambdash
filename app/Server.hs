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
import Lambdash.Repl (initial)

serve :: IO Int
serve = runServer ServerDefinition
  { parseConfig = const (const (Right ()))
  , onConfigChange = const (pure ())
  , defaultConfig = ()
  , configSection = "lambdash"
  , doInitialize = \env _ -> pure (Right env)
  , staticHandlers = const handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
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
  , requestHandler SMethod_TextDocumentHover $ \(TRequestMessage _ _ _ (HoverParams (TextDocumentIdentifier uri) (Position line column) _)) respond -> do
      found <- getVirtualFile (toNormalizedUri uri)
      let text = maybe "" (Text.unpack . virtualFileText) found
          -- The protocol counts from 0, and Check from 1.
          at = hoverAt initial text (fromIntegral line + 1) (fromIntegral column + 1)
      respond (Right (maybe (InR Null) (InL . hover) at))
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
      let problems = checkText initial (Text.unpack (virtualFileText file))
      publishDiagnostics maxProblems uri (Just (virtualFileVersion file)) (partitionBySource (map diagnostic problems))

-- | The type as code, in a block that the editor highlights as λsh.
hover :: (Int, Int, Int, String) -> Hover
hover (line, from, to, shown) =
  Hover (InL (MarkupContent MarkupKind_Markdown (Text.pack ("```lambdash\n" <> shown <> "\n```"))))
    (Just (Range (Position (fromIntegral (line - 1)) (fromIntegral (from - 1))) (Position (fromIntegral (line - 1)) (fromIntegral (to - 1)))))

maxProblems :: Int
maxProblems = 100

-- | Lines and columns count from 0 in the protocol, and from 1 in a
-- 'Problem'. A problem without a column covers its lines.
diagnostic :: Problem -> Diagnostic
diagnostic problem =
  -- Range, severity, code, code description, source, message, tags,
  -- related information, data.
  Diagnostic (Range start end) (Just DiagnosticSeverity_Error) Nothing Nothing (Just "lambdash")
    (Text.pack (problemMessage problem)) Nothing Nothing Nothing
 where
  line n = fromIntegral (n - 1)
  (start, end) = case problemColumn problem of
    Just column -> (Position (line (problemLine problem)) (fromIntegral (column - 1)), Position (line (problemLine problem)) (fromIntegral column))
    Nothing -> (Position (line (problemLine problem)) 0, Position (line (problemEndLine problem) + 1) 0)
