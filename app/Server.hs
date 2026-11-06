{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @lambdash --lsp@: a language server on stdin and stdout. It reports
-- problems, shows types on hover and after unsigned bindings, gives the
-- outline, goes to definitions and completes names. Each open file is checked as a startup file would run, but
-- nothing runs. See "Lambdash.Check" and "Lambdash.Navigate".
module Server (serve) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as Text
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText, virtualFileVersion)
import Lambdash.Check (Problem (..), checkText, hoverAt)
import Lambdash.Navigate
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
      { optTextDocumentSync = Just (TextDocumentSyncOptions (Just True) (Just TextDocumentSyncKind_Incremental) Nothing Nothing Nothing)
      -- After a dot: a module's names, or a record's fields.
      , optCompletionTriggerCharacters = Just ['.']
      }
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
      text <- textOf (toNormalizedUri uri)
      let (row, at') = fromPosition text (Position line column)
      at <- liftIO (hoverAt (startFor (toNormalizedUri uri)) text row at')
      respond (Right (maybe (InR Null) (InL . hover text) at))
  , requestHandler SMethod_TextDocumentDocumentSymbol $ \(TRequestMessage _ _ _ (DocumentSymbolParams _ _ (TextDocumentIdentifier uri))) respond -> do
      text <- textOf (toNormalizedUri uri)
      found <- liftIO (outline (startFor (toNormalizedUri uri)) text)
      respond (Right (InR (InL (map (documentSymbol text) found))))
  , requestHandler SMethod_TextDocumentDefinition $ \(TRequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier uri) position _ _)) respond -> do
      text <- textOf (toNormalizedUri uri)
      let (row, column) = fromPosition text position
      found <- liftIO (definitionAt (startFor (toNormalizedUri uri)) text row column)
      respond (Right (maybe (InR (InR Null)) (InL . Definition . InL . location uri) found))
  , requestHandler SMethod_TextDocumentCompletion $ \(TRequestMessage _ _ _ (CompletionParams (TextDocumentIdentifier uri) position _ _ _)) respond -> do
      text <- textOf (toNormalizedUri uri)
      let (row, column) = fromPosition text position
      (from, found) <- liftIO (completionsAt (startFor (toNormalizedUri uri)) text row column)
      let start = toPosition (lineOf text row) row from
      respond (Right (InL (map (completionItem (Range start position)) found)))
  , requestHandler SMethod_TextDocumentInlayHint $ \(TRequestMessage _ _ _ (InlayHintParams _ (TextDocumentIdentifier uri) (Range (Position first _) (Position final _)))) respond -> do
      text <- textOf (toNormalizedUri uri)
      found <- liftIO (inlayHints (startFor (toNormalizedUri uri)) text)
      let shown = [h | h <- found, let l = fromIntegral (hintLine h - 1), first <= l, l <= final]
      respond (Right (InL (map (inlayHint text) shown)))
  , notificationHandler SMethod_TextDocumentDidClose $ \(TNotificationMessage _ _ (DidCloseTextDocumentParams (TextDocumentIdentifier uri))) ->
      publishDiagnostics maxProblems (toNormalizedUri uri) Nothing (partitionBySource [])
  ]

-- | The file as the editor has it now.
textOf :: NormalizedUri -> LspM () String
textOf uri = maybe "" (Text.unpack . virtualFileText) <$> getVirtualFile uri

-- | The protocol counts from 0 in UTF-16 units, and Check from 1 in
-- characters.
fromPosition :: String -> Position -> (Int, Int)
fromPosition text (Position line column) =
  let row = fromIntegral line + 1
  in (row, fromUnits (lineOf text row) (fromIntegral column) + 1)

-- | A line and a column from 1, in characters, of a line's text, as a
-- protocol position.
toPosition :: String -> Int -> Int -> Position
toPosition source line column = Position (fromIntegral (line - 1)) (fromIntegral (toUnits source (column - 1)))

-- | A symbol covers the lines of its chunk, and its name is what the editor
-- selects.
documentSymbol :: String -> Symbol -> DocumentSymbol
documentSymbol text s = DocumentSymbol
  { _name = Text.pack (symbolName s)
  , _detail = Text.pack <$> symbolDetail s
  , _kind = case symbolSort s of
      ValueSymbol -> SymbolKind_Variable
      FunctionSymbol -> SymbolKind_Function
      TypeSymbol -> SymbolKind_Enum
      ConstructorSymbol -> SymbolKind_EnumMember
      ClassSymbol -> SymbolKind_Interface
      MethodSymbol -> SymbolKind_Method
      InstanceSymbol -> SymbolKind_Object
      ModuleSymbol -> SymbolKind_Module
  , _tags = Nothing
  , _deprecated = Nothing
  , _range = Range (Position (fromIntegral (symbolFirst s - 1)) 0) (toPosition final (symbolLast s) (length final + 1))
  , _selectionRange = Range (toPosition named (symbolLine s) (symbolFrom s)) (toPosition named (symbolLine s) (symbolTo s))
  , _children = if null (symbolChildren s) then Nothing else Just (map (documentSymbol text) (symbolChildren s))
  }
 where
  final = lineOf text (symbolLast s)
  named = lineOf text (symbolLine s)

-- | A type after a binding's name, as in its signature. A double click
-- writes the signature above the binding, where it checks.
inlayHint :: String -> Hint -> InlayHint
inlayHint text h = InlayHint
  { _position = toPosition (lineOf text (hintLine h)) (hintLine h) (hintColumn h)
  , _label = InL (Text.pack (": " <> hintType h))
  , _kind = Just InlayHintKind_Type
  , _textEdits = (\(line, signature) -> [TextEdit (Range (start line) (start line)) (Text.pack signature)]) <$> hintSignature h
  , _tooltip = InL "Double-click to write the signature" <$ hintSignature h
  , _paddingLeft = Just True
  , _paddingRight = Nothing
  , _data_ = Nothing
  }
 where
  start line = Position (fromIntegral (line - 1)) 0

-- | A definition in this file, or in a module's.
location :: Uri -> Place -> Location
location uri p = Location (maybe uri filePathToUri (placeFile p))
  (Range (toPosition (placeSource p) (placeLine p) (placeFrom p)) (toPosition (placeSource p) (placeLine p) (placeTo p)))

-- | A suggestion replaces the whole word before the cursor, with its
-- module's prefix, so the editor filters on that too.
completionItem :: Range -> Suggestion -> CompletionItem
completionItem range suggestion = CompletionItem
  { _label = shown
  , _labelDetails = Nothing
  , _kind = Just $ case suggestionSort suggestion of
      ValueSuggestion -> CompletionItemKind_Variable
      FunctionSuggestion -> CompletionItemKind_Function
      ConstructorSuggestion -> CompletionItemKind_EnumMember
      FieldSuggestion -> CompletionItemKind_Field
      KeywordSuggestion -> CompletionItemKind_Keyword
      WordSuggestion -> CompletionItemKind_Text
  , _tags = Nothing
  , _detail = Text.pack <$> suggestionDetail suggestion
  , _documentation = Nothing
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Just shown
  , _insertText = Nothing
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Just (InL (TextEdit range shown))
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }
 where
  shown = Text.pack (suggestionText suggestion)

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
