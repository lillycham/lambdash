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
  , Lens (..)
  , lenses
  ) where

import Control.Exception (IOException, try)
import Data.Char (isUpper)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe)
import Lambdash.Check (Problem, Use (..), identifierChar, operatorChar, useAt, walk)
import Lambdash.Complete (Candidate (..), completeField, isNameChar, namesStarting)
import Lambdash.Infer (elaborateRunWith, inferWith)
import Lambdash.Parser (reservedNames)
import Lambdash.Repl (Command (..), Session (..), initial, modulePath, parseCommandWith, renderEffectRow, renderEffectsType, sessionBound)
import Lambdash.Syntax (Arg (..), Declaration (..), Expr (..), Name, Span (..), patternVars)
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
-- chunk binds inside itself, such as a parameter, goes to its binder in the
-- chunk: see 'bindingAt'. A builtin goes nowhere.
definitionAt :: Session -> String -> Int -> Int -> IO (Maybe Place)
definitionAt start text line column = do
  walked <- walk start text
  let found = map entry walked
  case useAt walked text line column of
    Nothing -> pure Nothing
    Just use
      | Just command <- useCommand use
      , bindings <- [b | e <- expressionsOf command, let b = bindingAt (useChunk use) (useName use) (offsetIn use) e, b /= Free]
      , binding : _ <- bindings ->
          pure $ case binding of
            Local from _ -> Just (localPlace use from)
            _ -> Nothing
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

-- | The expressions of a chunk, where its names are used.
expressionsOf :: Command -> [Expr]
expressionsOf = \case
  Run e -> [e]
  TypeOf e -> [e]
  ShowCore e -> [e]
  ShowElab _ e -> [e]
  Bind _ e -> [e]
  BindRec group -> map snd group
  BindRun _ e -> [e]
  _ -> []

-- | Where the cursor of a use is, as an offset into its chunk.
offsetIn :: Use -> Int
offsetIn use =
  let before = take (line - useChunkLine use) (lines (useChunk use))
      line = useLine use
  in sum (map ((+ 1) . length) before) + useColumn use - 1

-- | A binder in a use's chunk, at an offset of the chunk, as a place.
localPlace :: Use -> Int -> Place
localPlace use at =
  let chunkLines = lines (useChunk use)
      before = takeWhile (<= at) (scanl (+) 0 (map ((+ 1) . length) chunkLines))
      row = length before
      column = at - last before + 1
  in Place Nothing (useChunkLine use + row - 1) column (column + length (useName use)) (concat (take 1 (drop (row - 1) chunkLines)))

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

-- | A line or block that runs: its first and last lines, and the effects
-- it may have, as its type gives them, or none.
data Lens = Lens
  { lensFirst :: Int
  , lensLast :: Int
  , lensEffects :: String
  }
  deriving (Eq, Show)

-- | A lens for each line or block that runs something, as the file runs:
-- an expression, or a @<-@ binding. A line that fails, a definition and a
-- declaration get none.
lenses :: Session -> String -> IO [Lens]
lenses start text = mapMaybe lensOf <$> walk start text
 where
  lensOf (n, chunk, before, _, Nothing) = do
    command <- either (const Nothing) Just (parseCommandWith (sessionFixities before) (sessionBound before) chunk)
    expression <- case command of
      Run e -> Just e
      BindRun _ e -> Just e
      _ -> Nothing
    -- The chunk checked, so one that does not elaborate as an action is a
    -- value, which the REPL prints.
    let row = case elaborateRunWith (sessionClasses before) (sessionTypes before) expression of
          Right (action, _, _) -> case snd (unclass (snd (unqualify action))) of
            TComp effects _ -> renderEffectRow effects
            TStream effects _ _ _ -> renderEffectRow effects
            TJob effects _ -> renderEffectRow effects
            _ -> ""
          Left _ -> ""
    pure (Lens n (n + length (lines chunk) - 1) (if null row then "pure" else row))
  lensOf _ = Nothing

-- | What a name in a chunk refers to, for the name under an offset of the
-- chunk: a binder in the chunk itself (its first offset and the offset
-- after it), a binder whose place is not known, or nothing in the chunk.
data Binding = Local Int Int | Unplaced | Free
  deriving (Eq, Show)

-- | The binder of a name in a chunk, by the scopes of the chunk's parsed
-- expressions. The parser gives each expression its span; a binder has no
-- span of its own, but its text comes just before what it scopes over: a
-- parameter before its lambda's body, @x@ in @let x = e@ or @x <- e@ before
-- @e@, a pattern variable before its arm's body. So a binder is the last
-- place of its name before that, and the binders of one lambda or pattern
-- come in order, each before the next. A cursor on a binder is on its own
-- definition.
bindingAt :: String -> Name -> Int -> Expr -> Binding
bindingAt chunk name cursor expression = fromMaybe Free (go Map.empty False expression)
 where
  -- With the binders in scope, and whether the expression has a span that
  -- holds the cursor: Nothing when the cursor is not in this expression.
  go :: Map.Map Name Binding -> Bool -> Expr -> Maybe Binding
  go scope located = \case
    ELoc (Span from to) inner
      | cursor < from || cursor >= to -> Nothing
      | otherwise -> go scope True inner
    EVar used
      | located && used == name -> Just (Map.findWithDefault Free used scope)
      | otherwise -> Nothing
    ECmd _ args
      | located && name `elem` [n | AVar n <- args] -> Just (Map.findWithDefault Free name scope)
      | otherwise -> Nothing
    lambda@(ELam _ _) ->
      let (names, body) = parameters lambda
      in binding scope names body (\inner -> go inner False body)
    ELet bound value body ->
      first [go scope False value, binding scope [bound] value (\inner -> go inner False body)]
    ELetRec group body ->
      let places = concat [placesBefore [n] e | (n, e) <- group]
          inner = foldr (uncurry Map.insert) scope places
      in first (onBinder places : go inner False body : [go inner False e | (_, e) <- group])
    EBind bound value body ->
      first [go scope False value, binding scope [bound] value (\inner -> go inner False body)]
    ECase scrutinee arms ->
      first (go scope False scrutinee : [binding scope (patternVars p) body (\inner -> go inner False body) | (p, body) <- arms])
    other -> first (map (go scope False) (subexpressions other))

  -- Bind names whose text comes before an expression, then go on.
  binding scope names before continue =
    let places = placesBefore names before
    in first [onBinder places, continue (foldr (uncurry Map.insert) scope places)]
  onBinder places = listToMaybe [b | (n, b@(Local from to)) <- places, n == name, from <= cursor, cursor < to]
  first = listToMaybe . catMaybes

  -- The names' binders, the last first: each is the last place of its name
  -- before the next one, or before the expression.
  placesBefore names before = case startOf before of
    Nothing -> [(n, Unplaced) | n <- names]
    Just limit -> snd (foldr settle (Just limit, []) names)
  -- A name that is not in the text, such as the one desugaring gives a
  -- parameter written `_`, leaves the others where they are.
  settle n (limit, found) = case limit >>= lastBefore n of
    Just at -> (Just at, (n, Local at (at + length n)) : found)
    Nothing -> (limit, (n, Unplaced) : found)
  lastBefore n limit = case [at | at <- wordOffsets n chunk, at + length n <= limit] of
    [] -> Nothing
    found -> Just (last found)

-- | A lambda's parameters and its body: @x y -> b@ is two lambdas.
parameters :: Expr -> ([Name], Expr)
parameters (ELam n body) = let (ns, inner) = parameters body in (n : ns, inner)
parameters body = ([], body)

-- | Where an expression's text starts: its first span, from the left.
startOf :: Expr -> Maybe Int
startOf (ELoc (Span from _) _) = Just from
startOf e = listToMaybe (mapMaybe startOf (subexpressions e))

-- | The expressions inside an expression, in the order of the text.
subexpressions :: Expr -> [Expr]
subexpressions = \case
  ELam _ b -> [b]
  EApp f x -> [f, x]
  EPipe a b -> [a, b]
  ELet _ a b -> [a, b]
  ELetRec group b -> map snd group <> [b]
  ERecord fields -> map snd fields
  EList items -> items
  ESelect e _ -> [e]
  EBind _ a b -> [a, b]
  ERun e -> [e]
  ELoc _ e -> [e]
  EWith a b -> [a, b]
  EBg e -> [e]
  EAnn e _ -> [e]
  EIf c a b -> [c, a, b]
  ECase s arms -> s : map snd arms
  _ -> []

-- | Each offset of a name in a text, as a whole word.
wordOffsets :: String -> String -> [Int]
wordOffsets n text =
  [ at
  | at <- [0 .. length text - length n]
  , n `isPrefixOf` drop at text
  , at == 0 || not (part (text !! (at - 1)))
  , maybe True (not . part) (listToMaybe (drop (at + length n) text))
  ]
 where
  part ch = identifierChar ch || ch == '.' || ch == '$'
