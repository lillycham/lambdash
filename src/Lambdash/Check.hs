-- | Check a file without running it, for an editor. Each line is parsed and
-- typed as the REPL would, in a session that grows as the file goes:
-- bindings, signatures, fixities and declarations. Nothing evaluates, nothing
-- spawns and no flake activates, so checking a file is always safe.
module Lambdash.Check
  ( Problem (..)
  , checkText
  , hoverAt
  , chunks
  ) where

import Data.Char (isAlphaNum, isDigit, isLetter)
import Data.List (intercalate)
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Eval (Value, typeEnv)
import Lambdash.Infer
import Lambdash.Parser (Bound (..))
import Lambdash.Repl (Command (..), Session (..), declareIn, needsMore, parseCommandWith, renderEffectsType, renderError, writtenCheck)
import Lambdash.Syntax (patternVars)

-- | Something wrong in a file. Lines and columns count from 1. Without a
-- column, the problem is the whole of its lines.
data Problem = Problem
  { problemLine :: Int
  , problemColumn :: Maybe Int
  , problemEndLine :: Int
  , problemMessage :: String
  }
  deriving (Eq, Show)

-- | The file's lines grouped as the REPL reads them: a @do@ block or a
-- @case@ that is still open takes the next line too. Each chunk has the
-- number of its first line.
chunks :: String -> [(Int, String)]
chunks = go . zip [1 ..] . lines
 where
  go [] = []
  go ((n, line) : rest) = gather n line rest
  gather n acc rest
    | needsMore acc, (_, next) : more <- rest = gather n (acc <> "\n" <> next) more
    | otherwise = (n, acc) : go rest

-- | The file as it goes: each chunk's first line, its text, the session
-- before it and after it, and its problem. A chunk that fails changes
-- nothing, and the chunks after it are still checked, as in a startup file.
walk :: Session -> String -> [(Int, String, Session, Session, Maybe Problem)]
walk start = go start . chunks
 where
  go _ [] = []
  go session ((n, chunk) : rest) = case checkChunk session chunk of
    Left problem -> (n, chunk, session, session, Just (locate n chunk problem)) : go session rest
    Right next -> (n, chunk, session, next, Nothing) : go next rest
  -- A parse error knows its place; a type error, for now, is its chunk.
  locate n chunk (Left (line, column, message)) =
    let text = concat (take 1 (drop (line - 1) (lines chunk)))
    in Problem (n + line - 1) (Just (characterColumn text column)) (n + line - 1) message
  locate n chunk (Right message) = Problem n Nothing (n + length (lines chunk) - 1) message

-- | Megaparsec gives a column with each tab as far as the next multiple of
-- eight. A 'Problem' counts characters, as 'hoverAt' does.
characterColumn :: String -> Int -> Int
characterColumn text wanted = go 1 1 text
 where
  go visual chars rest
    | visual >= wanted = chars
    | otherwise = case rest of
        '\t' : more -> go (visual + 8 - (visual - 1) `mod` 8) (chars + 1) more
        _ : more -> go (visual + 1) (chars + 1) more
        [] -> chars + (wanted - visual)

-- | Every problem in a file, in order.
checkText :: Session -> String -> [Problem]
checkText start text = [problem | (_, _, _, _, Just problem) <- walk start text]

-- | The name at a line and column, and its type there: the place of the
-- name (its line, first column and the column after it) and the text to
-- show. Lines and columns count from 1.
--
-- A name that a line uses has the type it had before the line, so the @x@ on
-- the right of @x = if x then 1 else 2@ is the earlier @x@. A name that the
-- line binds, where it binds it, has its new type: the @x@ on the left, a
-- member of a @let rec@ group, or what a declaration declares. A name that
-- the chunk binds inside itself, such as a lambda's
-- parameter, shows nothing: its type is not in the session, and the
-- session's type for the same name would be another name's.
hoverAt :: Session -> String -> Int -> Int -> Maybe (Int, Int, Int, String)
hoverAt start text line column = do
  (begins, chunk, before, after, _) <- case [c | c@(n, chunk, _, _, _) <- walk start text, n <= line, line < n + length (lines chunk)] of
    found : _ -> Just found
    [] -> Nothing
  source <- case drop (line - 1) (lines text) of
    l : _ -> Just l
    [] -> Nothing
  (from, to, name) <- wordAt source column
  let parsed = parseCommandWith (sessionFixities before) (BoundWith (Map.keysSet (sessionTypes before)) Set.empty) chunk
      -- The first word of the chunk, where a binding names what it binds.
      atHead = line == begins && from == 1 + length (takeWhile (== ' ') source)
      binds = case parsed of
        Right (Bind bound _) -> bound == name && atHead
        Right (BindRun bound _) -> bound == name && atHead
        Right (BindRec group) -> name `elem` map fst group
        Right (Declare _) -> True
        _ -> False
  case parsed of
    Right command | name `elem` locals command -> Nothing
    _ -> pure ()
  Forall _ ty <- Map.lookup name (sessionTypes (if binds then after else before))
  pure (line, from, to, shown name <> " : " <> renderEffectsType ty)
 where
  shown name
    | all operatorChar name = "(" <> name <> ")"
    | otherwise = name

-- | The identifier or operator under a column, with its first column and
-- the column after it.
wordAt :: String -> Int -> Maybe (Int, Int, String)
wordAt source column = case drop (column - 1) source of
  c : _
    | identifierChar c -> spanning identifierChar
    | operatorChar c -> spanning operatorChar
  _ -> Nothing
 where
  spanning ok =
    let from = column - length (takeWhile ok (reverse (take (column - 1) source)))
        to = column + length (takeWhile ok (drop (column - 1) source))
        word = take (to - from) (drop (from - 1) source)
    in if all operatorChar word || maybe False isLetter (headOf word) then Just (from, to, word) else Nothing
  headOf (c : _) = Just c
  headOf [] = Nothing

identifierChar, operatorChar :: Char -> Bool
identifierChar c = isAlphaNum c || c == '_' || c == '\''
operatorChar c = c `elem` ("!%&*+-/<=>?@^|~:" :: String)

-- | The names a line binds inside itself: parameters, let and bind names,
-- and pattern variables. A top-level binding's own name is not one.
locals :: Command -> [Name]
locals parsed = case parsed of
  Run e -> inner e
  TypeOf e -> inner e
  ShowCore e -> inner e
  ShowElab _ e -> inner e
  Bind _ e -> inner e
  BindRec group -> concatMap (inner . snd) group
  BindRun _ e -> inner e
  _ -> []
 where
  inner expression = case expression of
    ELam name body -> name : inner body
    ELet name bound body -> name : inner bound <> inner body
    ELetRec group body -> map fst group <> concatMap (inner . snd) group <> inner body
    EBind name e body -> name : inner e <> inner body
    ECase scrutinee arms -> inner scrutinee <> concat [patternVars p <> inner body | (p, body) <- arms]
    EApp f x -> inner f <> inner x
    EPipe a b -> inner a <> inner b
    ERecord fields -> concatMap (inner . snd) fields
    EList items -> concatMap inner items
    ESelect e _ -> inner e
    ERun e -> inner e
    ELoc _ e -> inner e
    EWith o e -> inner o <> inner e
    EBg e -> inner e
    EAnn e _ -> inner e
    EIf c a b -> inner c <> inner a <> inner b
    _ -> []
-- | One chunk: the session after it, or what is wrong. A parse error has a
-- line and column in the chunk.
checkChunk :: Session -> String -> Either (Either (Int, Int, String) String) Session
checkChunk session chunk =
  case parseCommandWith (sessionFixities session) (BoundWith (Map.keysSet types) builtinNames) chunk of
    Left err -> Left (Left (parseError err))
    Right parsed -> either (Left . Right . renderError) Right (typed parsed)
 where
  types = sessionTypes session
  classes = sessionClasses session
  builtinNames = Map.keysSet (Map.filterWithKey (\name scheme -> Map.lookup name typeEnv == Just scheme) types)
  typed parsed = case parsed of
    Run expression -> session <$ elaborateWith classes Defaulting types expression
    TypeOf expression -> session <$ inferWith classes types expression
    ShowElab _ expression -> session <$ elaborateWith classes Defaulting types expression
    Bind name expression -> do
      (ty, _) <- maybe (elaborateWith classes Generalising types expression)
        (\sig -> elaborateSigned classes types sig expression)
        (Map.lookup name (sessionSignatures session))
      pure (bind name ty (unsigned name session))
    BindRec group -> do
      done <- traverse (\(name, _) -> (,) name <$> maybe
        (elaborateWith classes Generalising types (ELetRec group (EVar name)))
        (\sig -> elaborateSigned classes types sig (ELetRec group (EVar name)))
        (Map.lookup name (sessionSignatures session))) group
      pure (foldl (\now (name, (ty, _)) -> bind name ty (unsigned name now)) session done)
    BindRun name expression -> do
      (_, ty, _) <- elaborateRunWith classes types expression
      pure (bind name ty session)
    Declare written -> fst <$> declareIn session written
    -- As at the prompt: a signature that names no class, or constrains
    -- another unknown, is wrong where it is written.
    Signature name context written -> case writtenCheck classes context written of
      Just problem -> Left (BadDeclaration problem)
      Nothing -> pure session {sessionSignatures = Map.insert name (context, written) (sessionSignatures session)}
    SetFixity fixity names ->
      pure session {sessionFixities = foldr (`Map.insert` fixity) (sessionFixities session) names}
    _ -> pure session
  -- Only the type matters here. The value is never asked for, because
  -- nothing evaluates.
  bind name ty now = now
    { sessionTypes = Map.insert name (generalise (sessionTypes now) ty) (sessionTypes now)
    , sessionValues = Map.Lazy.insert name unevaluated (sessionValues now)
    }
  unsigned name now = now {sessionSignatures = Map.delete name (sessionSignatures now)}
  unevaluated :: Value
  unevaluated = error "Lambdash.Check: a checked file never evaluates"

-- | Megaparsec's report starts @<input>:LINE:COLUMN:@. The rest is the
-- message, with the source line and a caret.
parseError :: String -> (Int, Int, String)
parseError report = case lines report of
  first : rest
    | (line, ':' : afterLine) <- span isDigit (drop 1 (dropWhile (/= ':') first))
    , (column, _) <- span isDigit afterLine
    , not (null line), not (null column) ->
        (read line, read column, intercalate "\n" (dropWhile excerpt rest))
  _ -> (1, 1, report)
 where
  -- The source line and the caret under it: an editor shows the place itself.
  excerpt text = case dropWhile (== ' ') (dropWhile isDigit (dropWhile (== ' ') text)) of
    '|' : _ -> True
    "" -> True
    _ -> False
