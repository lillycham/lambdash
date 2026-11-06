{-# LANGUAGE LambdaCase #-}

-- | Finding one's way in a file, for an editor: what the file defines (its
-- outline), where a name is defined, and what can complete a word. All of it
-- comes from the walk that "Lambdash.Check" makes, so nothing runs. Lines
-- and columns count from 1, in characters.
module Lambdash.Navigate
  ( Symbol (..)
  , SymbolSort (..)
  , outline
  , Place (..)
  , definitionAt
  , Suggestion (..)
  , SuggestionSort (..)
  , completionsAt
  , Hint (..)
  , inlayHints
  ) where

import Control.Exception (IOException, try)
import Data.Char (isUpper)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Lambdash.Check (Problem, Use (..), identifierChar, locals, operatorChar, useAt, walk)
import Lambdash.Complete (Candidate (..), completeField, isNameChar, namesStarting)
import Lambdash.Infer (inferWith)
import Lambdash.Parser (reservedNames)
import Lambdash.Repl (Command (..), Session (..), initial, modulePath, parseCommandWith, renderEffectsType, sessionBound)
import Lambdash.Syntax (Declaration (..), Expr (..))
import Lambdash.Unify (Ty (..), unclass, unqualify)
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (takeDirectory)

-- | What a symbol is, for the editor's icon.
data SymbolSort
  = ValueSymbol
  | FunctionSymbol
  | TypeSymbol
  | ConstructorSymbol
  | ClassSymbol
  | MethodSymbol
  | InstanceSymbol
  | ModuleSymbol
  deriving (Eq, Show)

-- | Something a file defines: its name and sort, its type if it has one,
-- the place of its name, the lines of its chunk, and what it holds (a data
-- type's constructors, a class's methods).
data Symbol = Symbol
  { symbolName :: String
  , symbolSort :: SymbolSort
  , symbolDetail :: Maybe String
  , symbolLine :: Int
  , symbolFrom :: Int
  , symbolTo :: Int
  , symbolFirst :: Int
  , symbolLast :: Int
  , symbolChildren :: [Symbol]
  }
  deriving (Eq, Show)

-- | A chunk of the file, with what it defines.
data Entry = Entry
  { entryLine :: Int
  , entryBefore :: Session
  , entryCommand :: Maybe Command
  , entrySymbols :: [Symbol]
  }

entries :: Session -> String -> IO [Entry]
entries start text = map entry <$> walk start text

entry :: (Int, String, Session, Session, Maybe Problem) -> Entry
entry (n, chunk, before, after, problem) =
  let parsed = either (const Nothing) Just (parseCommandWith (sessionFixities before) (sessionBound before) chunk)
      -- A chunk that fails binds nothing, so it defines nothing.
      defined = case problem of
        Nothing -> maybe [] (symbolsOf n chunk after) parsed
        Just _ -> []
  in Entry n before parsed defined

-- | What a file defines, in order. A name defined twice is there twice.
outline :: Session -> String -> IO [Symbol]
outline start text = concatMap entrySymbols <$> entries start text

-- | The symbols that one chunk defines.
symbolsOf :: Int -> String -> Session -> Command -> [Symbol]
symbolsOf n chunk after command = case command of
  Bind name _ -> value name (firstAt name (1, 1))
  BindRun name _ -> value name (firstAt name (1, 1))
  BindRec group -> concat [value name (recAt name) | (name, _) <- group]
  Declare (DeclData name _ constructors) ->
    let children = concat [symbol ConstructorSymbol c (typeOf c) [] (firstAt c (behind "=")) | (c, _) <- constructors]
    in symbol TypeSymbol name Nothing children (firstAt name (1, 1))
  Declare (DeclClass _ name _ methods _) ->
    let children = concat [symbol MethodSymbol m (typeOf m) [] (firstAt m (behind "where")) | (m, _) <- methods]
    in symbol ClassSymbol name Nothing children (firstAt name (1, 1))
  -- An instance has no name of its own: the outline shows its head.
  Declare (DeclInstance {}) ->
    let first = concat (take 1 (lines chunk))
        indent = length (takeWhile (== ' ') first)
        shown = unwords (takeWhile (/= "where") (words first))
    in [Symbol shown InstanceSymbol Nothing n (indent + 1) (indent + 1 + length shown) n final []]
  Import (Just alias) path -> symbol ModuleSymbol alias (Just path) [] (firstAt alias (1, 1))
  _ -> []
 where
  chunkLines = lines chunk
  final = n + length chunkLines - 1
  symbol sort name detail children = \case
    Just (l, c) -> [Symbol name sort detail (n + l - 1) c (c + length name) n final children]
    Nothing -> []
  value name = symbol (if isFunction name then FunctionSymbol else ValueSymbol) name (typeOf name) []
  typed name = either (const Nothing) Just (inferWith (sessionClasses after) (sessionTypes after) (EVar name))
  typeOf name = renderEffectsType <$> typed name
  isFunction name = case snd . unclass . snd . unqualify <$> typed name of
    Just (TFun _ _) -> True
    _ -> False
  -- The first place of a name at or after a place.
  firstAt name from = listToMaybe [p | p <- occurrences name chunkLines, p >= from]
  -- Where a word ends, as a place to look from: the constructors come after
  -- the @=@ of a data type, and the methods after the @where@ of a class.
  behind word = fromMaybe (1, 1) (listToMaybe [(l, c + length word) | (l, c) <- occurrences word chunkLines])
  -- A group's member is named after @rec@ or @and@. Its first place may be
  -- a use, in the body of a member before it.
  recAt name = case [p | p@(l, c) <- occurrences name chunkLines, after' l c] of
    p : _ -> Just p
    [] -> firstAt name (1, 1)
  after' l c =
    let before = reverse (dropWhile (== ' ') (reverse (take (c - 1) (chunkLines !! (l - 1)))))
    in any (\w -> (" " <> w) `isSuffixOf` (" " <> before)) ["rec", "and"]

-- | Each place of a name in some lines, as a whole word: line and column.
occurrences :: String -> [String] -> [(Int, Int)]
occurrences name chunkLines =
  [ (l, c)
  | (l, text) <- zip [1 ..] chunkLines
  , c <- [1 .. length text]
  , name `isPrefixOf` drop (c - 1) text
  , c == 1 || edge (text !! (c - 2))
  , maybe True edge (listToMaybe (drop (c - 1 + length name) text))
  ]
 where
  part = if all operatorChar name then operatorChar else \ch -> identifierChar ch || ch == '.'
  edge = not . part

-- | Where something is defined: a file (none for this one), its line, the
-- first column and the column after, and the text of that line.
data Place = Place
  { placeFile :: Maybe FilePath
  , placeLine :: Int
  , placeFrom :: Int
  , placeTo :: Int
  , placeSource :: String
  }
  deriving (Eq, Show)

-- | Where the name at a line and column is defined. A use goes to the
-- definition that it sees, the latest before its chunk, so the @x@ on the
-- right of @x = x + 1@ goes to the earlier @x@. A name a module brought in
-- goes to the module's file, through other modules if need be. A name that a
-- chunk binds inside itself, such as a parameter, and a builtin, go nowhere.
definitionAt :: Session -> String -> Int -> Int -> IO (Maybe Place)
definitionAt start text line column = do
  walked <- walk start text
  let found = map entry walked
  case useAt walked text line column of
    Nothing -> pure Nothing
    Just use
      | Just command <- useCommand use, useName use `elem` locals command -> pure Nothing
      | otherwise -> do
          let name = useName use
              earlier = takeWhile ((< useChunkLine use) . entryLine) found
              here = [e | e <- found, entryLine e == useChunkLine use]
              -- The chunk's own names count where they are defined, and in a
              -- group or a declaration, which can name themselves.
              own = concat
                [ entrySymbols e
                | e <- here
                , selfNaming (entryCommand e) || any onSite (flatten (entrySymbols e))
                ]
              onSite s = symbolLine s == line && symbolFrom s <= column && column < symbolTo s
              seen = concatMap (flatten . entrySymbols) earlier <> flatten own
          case reverse [s | s <- seen, symbolName s == name, symbolSort s /= InstanceSymbol] of
            s : _ -> pure (Just (place Nothing text s))
            [] -> imported [] (reverse [(entryBefore e, alias, path) | e <- earlier, Just (Import alias path) <- [entryCommand e]]) name
 where
  selfNaming = \case
    Just (BindRec _) -> True
    Just (Declare _) -> True
    _ -> False

-- | The symbols and what they hold, in order.
flatten :: [Symbol] -> [Symbol]
flatten = concatMap (\s -> s : flatten (symbolChildren s))

place :: Maybe FilePath -> String -> Symbol -> Place
place file text s = Place file (symbolLine s) (symbolFrom s) (symbolTo s) (concat (take 1 (drop (symbolLine s - 1) (lines text))))

-- | A name looked for in modules, the latest import first. Under a prefix,
-- @p.name@ is the module's @name@. A class and its methods are not
-- prefixed, so an unprefixed name may be one from a prefixed import.
imported :: [FilePath] -> [(Session, Maybe String, FilePath)] -> String -> IO (Maybe Place)
imported _ [] _ = pure Nothing
imported visited ((session, alias, written) : rest) name = do
  inModule <- case alias of
    Just prefix | (prefix <> ".") `isPrefixOf` name -> look (drop (length prefix + 1) name) (const True)
    Just _ -> look name (`elem` [ClassSymbol, MethodSymbol])
    Nothing -> look name (const True)
  maybe (imported visited rest name) (pure . Just) inModule
 where
  look wanted sortOk = do
    path <- modulePath session written
    exists <- doesFileExist path
    if not exists
      then pure Nothing
      else do
        canonical <- canonicalizePath path
        loaded <- try (readFile canonical)
        case loaded of
          Left (_ :: IOException) -> pure Nothing
          Right text
            | canonical `elem` visited -> pure Nothing
            | otherwise -> do
                let moduleStart = initial
                      { sessionRoots = sessionRoots session
                      , sessionDirectory = Just (takeDirectory canonical)
                      , sessionImporting = canonical : visited
                      }
                found <- entries moduleStart text
                case reverse [s | s <- concatMap (flatten . entrySymbols) found, symbolName s == wanted, sortOk (symbolSort s)] of
                  s : _ -> pure (Just (place (Just canonical) text s))
                  [] -> imported (canonical : visited) (reverse [(entryBefore e, a, p) | e <- found, Just (Import a p) <- [entryCommand e]]) wanted

-- | What a suggestion is, for the editor's icon.
data SuggestionSort
  = ValueSuggestion
  | FunctionSuggestion
  | ConstructorSuggestion
  | FieldSuggestion
  | KeywordSuggestion
  | WordSuggestion
  deriving (Eq, Show)

-- | A completion: the text that replaces the word, its sort, and its type.
data Suggestion = Suggestion
  { suggestionText :: String
  , suggestionSort :: SuggestionSort
  , suggestionDetail :: Maybe String
  }
  deriving (Eq, Show)

-- | What can complete the word before a line and column, and the column
-- where that word starts. The names are those the line sees: the session
-- before its chunk. After a dot come a record's fields or a module's names,
-- as at the prompt. Otherwise the reserved words come too, and the words of
-- the chunk itself, such as its parameters, which no session holds.
completionsAt :: Session -> String -> Int -> Int -> IO (Int, [Suggestion])
completionsAt start text line column = do
  walked <- walk start text
  let source = concat (take 1 (drop (line - 1) (lines text)))
      word = reverse (takeWhile isNameChar (reverse (take (column - 1) source)))
      from = column - length word
      containing = [(c, before) | (n, c, before, _, _) <- walked, n <= line, line < n + length (lines c)]
      (chunk, session) = case containing of
        found : _ -> found
        [] -> ("", maybe start (\(_, _, _, after, _) -> after) (lastOf walked))
      named = [suggest c | c <- namesStarting session word]
      dotted = '.' `elem` word
      fields = [Suggestion t FieldSuggestion (Just h) | Candidate t h <- completeField session word]
      keywords = [Suggestion k KeywordSuggestion Nothing | k <- reservedNames, word `isPrefixOf` k]
      known = Map.keysSet (sessionTypes session)
      inChunk =
        [ Suggestion w WordSuggestion Nothing
        | w <- nub (wordsOf chunk)
        , word `isPrefixOf` w, w /= word
        , w `notElem` reservedNames, not (w `elem` known)
        ]
  pure (from, if dotted then fields <> named else named <> keywords <> inChunk)
 where
  suggest (Candidate name shown) =
    let sort'
          | maybe False isUpper (listToMaybe (dropModule name)) = ConstructorSuggestion
          | " -> " `isInfixOf` shown = FunctionSuggestion
          | otherwise = ValueSuggestion
    in Suggestion name sort' (if null shown then Nothing else Just shown)
  dropModule name = reverse (takeWhile (/= '.') (reverse name))
  lastOf xs = if null xs then Nothing else Just (last xs)
  wordsOf = mapMaybe identifierWord . splitWords
  splitWords s = case dropWhile (not . identifierChar) s of
    [] -> []
    rest -> let (w, more) = span identifierChar rest in w : splitWords more
  identifierWord w = case w of
    c : _ | c `elem` ['a' .. 'z'] || c == '_' -> Just w
    _ -> Nothing

-- | A binding's type, shown after its name: the line and the column after
-- the name, the type, and the signature that would say it, as a line to
-- put above the binding's chunk, with the line it goes on.
data Hint = Hint
  { hintLine :: Int
  , hintColumn :: Int
  , hintType :: String
  , hintSignature :: Maybe (Int, String)
  }
  deriving (Eq, Show)

-- | A hint for each binding that has no signature. A signature comes with a
-- hint only where it checks: written above the binding, the binding still
-- checks against it. An operator's binding and a @<-@ binding get none,
-- since no signature line names them.
inlayHints :: Session -> String -> IO [Hint]
inlayHints start text = do
  walked <- walk start text
  concat <$> mapM hintsOf walked
 where
  hintsOf walked@(n, chunk, before, _, _) =
    let e = entry walked
        unsigned = [s | s <- entrySymbols e, symbolSort s `elem` [ValueSymbol, FunctionSymbol], Map.notMember (symbolName s) (sessionSignatures before)]
    in case entryCommand e of
      Just (Bind _ _) -> catMaybes <$> mapM (hint True n chunk before) unsigned
      Just (BindRec _) -> catMaybes <$> mapM (hint True n chunk before) unsigned
      Just (BindRun _ _) -> catMaybes <$> mapM (hint False n chunk before) unsigned
      _ -> pure []
  hint signable n chunk before s = case symbolDetail s of
    Nothing -> pure Nothing
    Just ty -> do
      let signature = symbolName s <> " : " <> ty
      ok <- if signable && not (all operatorChar (symbolName s))
        then checks before (signature <> "\n" <> chunk)
        else pure False
      pure (Just (Hint (symbolLine s) (symbolTo s) ty (if ok then Just (n, signature <> "\n") else Nothing)))
  checks before candidate = all (\(_, _, _, _, problem) -> isNothing problem) <$> walk before candidate
