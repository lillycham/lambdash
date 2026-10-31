-- | Types, substitutions and unification: the first piece of the checker.
--
-- A substitution is a solution so far: what each unknown has turned out to be.
-- Unification takes two types and finds the substitution that makes them equal.
module Lambdash.Unify
  ( Var (..)
  , Ty (..)
  , Subst
  , TypeError (..)
  , emptySubst
  , singleton
  , apply
  , compose
  , occurs
  , bind
  , unify
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | An unknown type. @infer@ will make fresh ones by counting.
newtype Var = Var Int
  deriving (Eq, Ord, Show)

data Ty
  = TVar Var
  | TFun Ty Ty
  | TInt
  | TBool
  deriving (Eq, Show)

newtype Subst = Subst (Map Var Ty)
  deriving (Eq, Show)

data TypeError
  = -- | Two types can never be equal.
    Mismatch Ty Ty
  | -- | The solution for this unknown would be an infinite type.
    Infinite Var Ty
  deriving (Eq, Show)

-- | The substitution that solves nothing.
emptySubst :: Subst
emptySubst = Subst Map.empty

-- | One solved unknown.
singleton :: Var -> Ty -> Subst
singleton v t = Subst (Map.singleton v t)

-- | Replace each solved unknown in a type.
apply :: Subst -> Ty -> Ty
apply = undefined

-- | Compose like functions: @apply (compose s1 s2) = apply s1 . apply s2@.
-- So @s2@ happens first. Note what that needs for the types already in @s2@.
compose :: Subst -> Subst -> Subst
compose = undefined

-- | Does this unknown appear anywhere in this type?
occurs :: Var -> Ty -> Bool
occurs = undefined

-- | Solve one unknown, or refuse.
bind :: Var -> Ty -> Either TypeError Subst
bind = undefined

-- | Find the substitution that makes two types equal.
--
-- Note: what you learn from one side of a 'TFun' has to apply to the other.
unify :: Ty -> Ty -> Either TypeError Subst
unify = undefined
