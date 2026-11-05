-- | Search the session's names by type, as Hoogle does: @:find [Int] -> [Int]@
-- finds @sort@ and @reverse@.
--
-- A name matches when its type is at least as general as the query: it can
-- be used where a value of the query's type is wanted. The signature
-- checker decides that, so the query means what a signature line means. Its
-- unknowns are rigid, its context must give every class the name needs, and
-- an effect row that the query does not write is open.
--
-- The arguments may also come in another order: @String -> Int -> String@
-- finds a function that takes the Int first.
module Lambdash.Find
  ( Found (..)
  , findByType
  ) where

import Data.List (permutations, sortOn)
import Data.Map.Strict qualified as Map
import Data.Either (isRight)
import Lambdash.Classes (ClassEnv)
import Lambdash.Infer (Env, Scheme (..), elaborateSigned)
import Lambdash.Syntax (Expr (..), Name, SPred, SType (..))
import Lambdash.Unify (Row (..), Ty (..))

-- | A name that matches, and whether its arguments had to change order.
data Found = Found
  { foundName :: Name
  , foundReordered :: Bool
  }
  deriving (Eq, Show)

-- | The names that match, best first: those that fit as they are before
-- those whose arguments change order, and then the more specific before
-- the more general, so that @reverse@ comes before @id@.
findByType :: ClassEnv -> Env -> ([SPred], SType) -> [Found]
findByType classes env query =
  map snd (sortOn fst
    [ ((reordered, negate (size ty), name), Found name reordered)
    | (name, Forall _ ty) <- Map.toList env
    , not (internal name)
    , Just reordered <- [matches name]
    ])
 where
  arity = length (arguments (snd query))
  -- Up to four arguments in every order: 24 checks a name at most.
  orders
    | arity >= 2 && arity <= 4 = drop 1 (permutations [0 .. arity - 1])
    | otherwise = []
  fits expression = isRight (elaborateSigned classes env query expression)
  matches name
    | fits (EVar name) = Just False
    | any (fits . reorder name) orders = Just True
    | otherwise = Nothing
  -- x0 x1 x2 -> name x2 x0 x1: the query's arguments, in another order.
  reorder name order =
    let params = ["%find" <> show i | i <- [0 .. arity - 1]]
    in foldr ELam (foldl EApp (EVar name) [EVar (params !! i) | i <- order]) params

-- | A name that only the checker uses: @%show@, @%instance Eq Int@. The
-- operator @%@ is not one.
internal :: Name -> Bool
internal name = take 1 name == "%" && not (all (`elem` ("!%&*+-/<=>?@^|~:" :: String)) name)

-- | The arguments of a written function type: @a -> b -> c@ has two.
arguments :: SType -> [SType]
arguments (STFun a rest) = a : arguments rest
arguments _ = []

-- | How much of a type is structure, not unknowns: the more, the more
-- specific the type.
size :: Ty -> Int
size ty = case ty of
  TVar _ -> 0
  TCon _ -> 1
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
 where
  rowSize (RExtend _ t rest) = 1 + size t + rowSize rest
  rowSize _ = 0
