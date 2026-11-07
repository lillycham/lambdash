-- | What could fill a hole. A fit is a text that, put where the hole is,
-- makes the line check: the checker decides, with the whole line around the
-- hole, so a fit is exactly what the line accepts there.
--
-- The candidates are the names in the hole's scope, and for a hole that
-- takes a record, its fields as accessors such as @.name@. A quick
-- unification against the hole's type drops most of them before the real
-- check. A refinement, @(f _)@ or @(f _ _)@, is a function whose result
-- fits: its new holes come back with their types.
--
-- This module does not check lines itself: the caller gives the check, so
-- that the prompt and the editor each use their own session.
module Lambdash.Fill
  ( Fit (..)
  , fitsFor
  , rankIn
  ) where

import Data.Char (isAlphaNum, isLetter)
import Data.Either (isRight)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Lambdash.Infer (Env, Hole (..), InferError (..), Name, Scheme (..), Span (..), ftv, ftvScheme, instantiate, unlocated)
import Lambdash.Unify (Row (..), Ty (..), Var (..), unclass, unify, unqualify)
import Control.Monad.Trans.State.Strict (runStateT)

-- | A text for a hole, the type of what it names, and for a refinement,
-- the holes it leaves.
data Fit = Fit
  { fitText :: String
  , fitType :: Ty
  , fitHoles :: [Hole]
  }
  deriving (Eq, Show)

-- | How many candidates get the real check, for names and for refinements.
-- The quick unification has already dropped most names.
plainChecks, refinedChecks :: Int
plainChecks = 40
refinedChecks = 20

-- | The fits for one hole of a line, best first: the names the line binds,
-- then the session's own, then the builtins; plain names before
-- refinements; and the more specific type before the more general.
--
-- The check takes a whole line of text. Other holes in the line stay holes,
-- and a line that fails only with holes counts as checked. Such a line
-- stops before its classes are resolved, so with other holes there, a fit
-- may still need an instance that is missing.
fitsFor :: (String -> Either InferError a) -> (Name -> Int) -> String -> Hole -> [Fit]
fitsFor check rank source hole = plain <> refined
 where
  Span from to = holeSpan hole
  filled text = take from source <> text <> drop to source
  goal = bare (holeType hole)
  scope = holeScope hole
  candidates = sortOn order ([(name, sc) | (name, sc) <- Map.toList scope, usable name] <> accessors)
  order (name, Forall _ ty) = (rank name, negate (size ty), name)

  plain =
    [ Fit name (bare ty) []
    | (name, Forall _ ty) <- take plainChecks [c | c@(_, sc) <- candidates, fitsAfter 0 sc]
    , passes (check (filled name))
    ]
  refined =
    [ Fit text (bare ty) [h | h <- holes, within (holeSpan h)]
    | (name, Forall _ ty, arguments) <- take refinedChecks
        [ (n, sc, k)
        | (n, sc) <- candidates, n `notElem` map fitText plain
        , k <- [1, 2], fitsAfter k sc
        ]
    , let text = "(" <> unwords (name : replicate arguments "_") <> ")"
          end = from + length text
          within (Span a _) = a >= from && a < end
    , Left err <- [check (filled text)]
    , Holes holes <- [unlocated err]
    , any (within . holeSpan) holes
    ]

  -- A record's fields, for a hole that takes the record: .name in
  -- ls "." | map _.
  accessors = case goal of
    TFun (TRecord row) _ -> [('.' : label, Forall [] (TFun (TRecord row) ty)) | (label, ty) <- fields row]
    _ -> []
  fields (RExtend label ty rest) = (label, ty) : fields rest
  fields _ = []

  -- Whether the name's type, after some arguments, can be the hole's type.
  -- Classes and effect constraints wait for the real check.
  fitsAfter :: Int -> Scheme -> Bool
  fitsAfter arguments sc = isRight (runStateT (instantiate sc >>= \t -> unify (result arguments (bare t)) goal) start)
  result 0 t = t
  result n (TFun _ t) = result (n - 1) t
  result _ t = t
  -- Unknowns above every one the hole and its scope use.
  start = 1 + maximum (0 : [n | Var n <- Set.toList (Set.union (ftv goal) (foldMap ftvScheme (Map.elems scope)))])

  passes (Right _) = True
  passes (Left err) = case unlocated err of
    Holes _ -> True
    _ -> False

-- | A type without its classes and effect constraints.
bare :: Ty -> Ty
bare = snd . unclass . snd . unqualify

-- | A name a line can write as it is: an identifier, maybe with a module's
-- prefix. Operators and the checker's own names, which start with @%@, are
-- not.
usable :: Name -> Bool
usable name = case name of
  c : rest -> isLetter c && all (\d -> isAlphaNum d || d `elem` ("_'." :: String)) rest
  [] -> False

-- | How big a type is, so that the more specific comes first: @Int@ before
-- @a@.
size :: Ty -> Int
size ty = case ty of
  TVar _ -> 0
  TFun a b -> 1 + size a + size b
  TList a -> 1 + size a
  TData _ ts -> 1 + sum (map size ts)
  TRecord row -> 1 + rowSize row
  TComp _ a -> 1 + size a
  TJob _ a -> 1 + size a
  TStream _ i o r -> 1 + size i + size o + size r
  TStage i o -> 1 + size i + size o
  TCollector i o -> 1 + size i + size o
  TQualified _ a -> size a
  TClass _ a -> size a
  TCon _ -> 1
 where
  rowSize (RExtend _ t rest) = 1 + size t + rowSize rest
  rowSize _ = 0

-- | The rank of a name for a hole: 0 for one the line binds itself, 1 for
-- one the session binds, 2 for a builtin. The first environment is the
-- session's, the second the builtins'.
rankIn :: Env -> Env -> Name -> Int
rankIn session builtins name
  | Map.notMember name session = 0
  | Map.member name builtins && Map.lookup name builtins == Map.lookup name session = 2
  | otherwise = 1
