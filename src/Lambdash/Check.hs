-- | Check a file without running it, for an editor. Each line is parsed and
-- typed as the REPL would, in a session that grows as the file goes:
-- bindings, signatures, fixities and declarations. Nothing evaluates, nothing
-- spawns and no flake activates, so checking a file is always safe.
module Lambdash.Check
  ( Problem (..)
  , checkText
  , chunks
  ) where

import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Lambdash.Declare (Declared (..), declare)
import Lambdash.Eval (Value, typeEnv)
import Lambdash.Infer
import Lambdash.Parser (Bound (..))
import Lambdash.Repl (Command (..), Session (..), needsMore, parseCommandWith, renderError)

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

-- | Every problem in a file, in order. A line that fails changes nothing, and
-- the lines after it are still checked, as in a startup file.
checkText :: Session -> String -> [Problem]
checkText start = go start . chunks
 where
  go _ [] = []
  go session ((n, chunk) : rest) = case checkChunk session chunk of
    Left problem -> locate n chunk problem : go session rest
    Right next -> go next rest
  -- A parse error knows its place; a type error, for now, is its chunk.
  locate n _ (Left (line, column, message)) = Problem (n + line - 1) (Just column) (n + line - 1) message
  locate n chunk (Right message) = Problem n Nothing (n + length (lines chunk) - 1) message

-- | One chunk: the session after it, or what is wrong. A parse error has a
-- line and column in the chunk.
checkChunk :: Session -> String -> Either (Either (Int, Int, String) String) Session
checkChunk session chunk =
  case parseCommandWith (sessionFixities session) (BoundWith (Map.keysSet types) builtinNames) chunk of
    Left err -> Left (Left (parseError err))
    Right parsed -> either (Left . Right . renderError) Right (typed parsed)
 where
  types = sessionTypes session
  values = sessionValues session
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
    Declare written -> do
      done <- declare classes types values written
      pure session
        { sessionClasses = declaredClasses done
        , sessionTypes = foldr (uncurry Map.insert) types (declaredTypes done)
        , sessionValues = foldr (uncurry Map.Lazy.insert) values (declaredValues done)
        }
    Signature name context written ->
      pure session {sessionSignatures = Map.insert name (context, written) (sessionSignatures session)}
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
