-- | What the prompt offers on Tab. The line editor asks for the word before
-- the cursor; this module says what can complete it, and leaves the editor
-- and the file system to "Main".
module Lambdash.Complete
  ( Candidate (..)
  , completeName
  , completeField
  , isNameChar
  ) where

import Data.Char (isAlphaNum)
import Data.List (intercalate, isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Lambdash.Help qualified as Help
import Lambdash.Infer (inferWith)
import Lambdash.Repl (Session (..), renderEffectsType, renderType)
import Lambdash.Unify (Row (..), Ty (..), unclass, unqualify)
import Lambdash.Syntax (Expr (..))

-- | One completion: the text that replaces the word, and what the menu shows
-- beside it.
data Candidate = Candidate
  { candidateText :: String
  , candidateHelp :: String
  }
  deriving (Eq, Show)

-- | The characters of a word that a name completes: an identifier's, and
-- the dot of a module's prefix, so that @tab.pa@ completes to
-- @tab.padRight@.
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c `elem` ("_'." :: String)

-- | The names in the session that start with a word, with their types, in
-- order. After @:help@ or @:h@, the help topics too. An empty word gives
-- nothing: every name at once is a list, not a completion.
--
-- Names that only the checker uses, which start with @%@, never show, and
-- neither do operators: no word can start one.
completeName :: Session -> String -> String -> [Candidate]
completeName session line word
  | null word = []
  | otherwise = topics <> names
 where
  names =
    [ Candidate name (typeOf name)
    | name <- Map.keys (sessionTypes session)
    , word `isPrefixOf` name
    , all isNameChar name
    ]
  topics
    | any (`isPrefixOf` line) [":help ", ":h "] =
        [Candidate name "topic" | name <- sort Help.topicNames, word `isPrefixOf` name]
    | otherwise = []
  -- As :t shows it: with its context, and its effects.
  typeOf name = either (const "") renderEffectsType
    (inferWith (sessionClasses session) (sessionTypes session) (EVar name))

-- | After a name and a dot, the fields of its record that start with what
-- follows the dot, with their types: @r.na@ gives @r.name@. A chain works
-- too, @r.owner.na@, and so does a module's binding, @tab.config.si@. The
-- type comes from the checker, so a record that a line built, or that a
-- command gave, completes as well as one written out.
completeField :: Session -> String -> [Candidate]
completeField session word = case break (== '.') (reverse word) of
  (_, []) -> []
  (wanted, '.' : before) ->
    let path = reverse before
        prefix = reverse wanted
    in case recordFields path of
      Nothing -> []
      Just fields ->
        [Candidate (path <> "." <> label) (renderType ty) | (label, ty) <- fields, prefix `isPrefixOf` label]
  _ -> []
 where
  types = sessionTypes session
  -- The longest start of the path that is a name, then a select for each
  -- part after it: tab.config.size is the name tab.config, then .size.
  recordFields path = do
    expression <- located (splitOn path)
    ty <- either (const Nothing) Just (inferWith (sessionClasses session) types expression)
    case snd (unclass (snd (unqualify ty))) of
      TRecord row -> Just (known row)
      _ -> Nothing
  located parts = case [n | n <- [length parts, length parts - 1 .. 1], Map.member (intercalate "." (take n parts)) types] of
    n : _ -> Just (foldl ESelect (EVar (intercalate "." (take n parts))) (drop n parts))
    [] -> Nothing
  known (RExtend label ty rest) = (label, ty) : known rest
  known _ = []
  splitOn text = case break (== '.') text of
    (part, []) -> [part]
    (part, _ : rest) -> part : splitOn rest
