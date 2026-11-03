-- | The classes and instances that the checker resolves against.
--
-- These are data, so that declarations at the prompt can extend them later.
-- For now they hold the prelude: 'Eq' and 'Ord' for the built-in types and
-- for lists. The dictionaries themselves are values in "Lambdash.Eval", under
-- the names that 'instanceName' gives.
module Lambdash.Classes
  ( ClassInfo (..)
  , Instance (..)
  , classes
  , instances
  , lookupInstance
  , superclassesOf
  , instanceName
  , tyKey
  , superField
  , recordFields
  , extendFields
  ) where

import Data.List (find, nub)
import Lambdash.Unify

data ClassInfo = ClassInfo
  { className :: String
  , -- | Direct superclasses. A dictionary holds one for each, so @Ord a@
    -- provides @Eq a@.
    classSupers :: [String]
  }

-- | An instance for a type constructor. Each argument of the constructor
-- must be an instance of the listed classes: @Eq [a]@ requires @Eq a@.
data Instance = Instance
  { instClass :: String
  , instHead :: String
  , instRequires :: [[String]]
  }

classes :: [ClassInfo]
classes =
  [ ClassInfo "Eq" []
  , ClassInfo "Ord" ["Eq"]
  ]

instances :: [Instance]
instances =
  [Instance "Eq" ty [] | ty <- ["Int", "Bool", "String", "()", "Status", "Bytes", "Patch", "Overlay"]]
    <> [Instance "Eq" "List" [["Eq"]]]
    -- Structural: a record is Eq when every field is. See 'recordFields'.
    <> [Instance "Eq" "Record" []]
    <> [Instance "Ord" ty [] | ty <- ["Int", "Bool", "String", "()"]]
    <> [Instance "Ord" "List" [["Ord"]]]

-- | The constructor of a type, for instance lookup, and its arguments.
tyKey :: Ty -> Maybe (String, [Ty])
tyKey (TCon name) = Just (name, [])
tyKey (TList item) = Just ("List", [item])
tyKey _ = Nothing

lookupInstance :: String -> String -> Maybe Instance
lookupInstance cls key = find (\i -> instClass i == cls && instHead i == key) instances

-- | Every superclass, transitively.
superclassesOf :: String -> [String]
superclassesOf cls = nub (concat [supers <> concatMap superclassesOf supers | ClassInfo name supers <- classes, name == cls])

-- | The name of an instance's dictionary. No identifier can spell it.
instanceName :: String -> String -> String
instanceName cls key = "%instance " <> cls <> " " <> key

-- | The field of a dictionary that holds the dictionary of a superclass.
superField :: String -> String
superField cls = '%' : cls

-- | The field of a record's dictionary that holds the dictionaries of its
-- fields, by label. A function that is Eq over an open row, @Eq { | r}@,
-- takes the dictionary for the tail and extends its fields with the ones it
-- knows.
recordFields :: String
recordFields = "%fields"

-- | The builtin that extends a record of field dictionaries with more.
extendFields :: String
extendFields = "%extendFields"
