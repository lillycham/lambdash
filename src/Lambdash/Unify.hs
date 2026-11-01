{-# LANGUAGE PatternSynonyms #-}

-- | Types, substitutions and unification: the first piece of the checker.
--
-- A substitution is a solution so far: what each unknown has turned out to be.
-- Unification takes two types and finds the substitution that makes them equal.
module Lambdash.Unify
  ( Var (..),
    Ty (..),
    pattern TInt,
    pattern TBool,
    Subst(..),
    TypeError (..),
    emptySubst,
    singleton,
    apply,
    compose,
    occurs,
    bind,
    unify,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | An unknown type. @infer@ will make fresh ones by counting.
newtype Var = Var Int
  deriving (Eq, Ord, Show)

data Ty
  = TVar Var
  | TFun Ty Ty
  | -- | A type with a name and no parts.
    TCon String
  deriving (Eq, Show)

pattern TInt, TBool :: Ty
pattern TInt = TCon "Int"
pattern TBool = TCon "Bool"

newtype Subst = Subst {getSubst :: Map Var Ty}
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
apply s (TVar v) = case Map.lookup v (getSubst s) of
  Nothing -> TVar v
  Just t -> t
apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
apply _ t = t

-- | Compose like functions: @apply (compose s1 s2) = apply s1 . apply s2@.
-- So @s2@ happens first. Note what that needs for the types already in @s2@.
compose :: Subst -> Subst -> Subst
compose s1@(Subst m1) (Subst m2) =
  Subst $ (Map.map (apply s1) m2) `Map.union` m1

-- | Does this unknown appear anywhere in this type?
occurs :: Var -> Ty -> Bool
occurs v (TVar v') = v == v'
occurs v (TFun t1 t2) = occurs v t1 || occurs v t2
occurs _ _ = False

-- | Solve one unknown, or refuse.
bind :: Var -> Ty -> Either TypeError Subst
bind v t
  | t == TVar v = Right emptySubst
  | occurs v t = Left (Infinite v t)
  | otherwise = Right (Subst (Map.singleton v t))

-- | Find the substitution that makes two types equal.
--
-- Note: what you learn from one side of a 'TFun' has to apply to the other.
unify :: Ty -> Ty -> Either TypeError Subst
unify t1 t2 | t1 == t2 = Right emptySubst
unify (TVar v) t = bind v t
unify t (TVar v) = bind v t
unify (TFun l1 r1) (TFun l2 r2) = do
  s1 <- unify l1 l2
  s2 <- unify (apply s1 r1) (apply s1 r2)
  Right (s2 `compose` s1)
unify t1 t2 = Left (Mismatch t1 t2)
