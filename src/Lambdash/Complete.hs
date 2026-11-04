-- | What the prompt offers on Tab. The line editor asks for the word before
-- the cursor; this module says what can complete it, and leaves the editor
-- and the file system to "Main".
module Lambdash.Complete
  ( Candidate (..)
  , completeName
  , isNameChar
  ) where

import Data.Char (isAlphaNum)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict qualified as Map
import Lambdash.Help qualified as Help
import Lambdash.Infer (inferWith)
import Lambdash.Repl (Session (..), renderEffectsType)
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
