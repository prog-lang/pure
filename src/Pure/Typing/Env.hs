{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Env
  ( Env (..),
    Subst,
    Context,
    empty,
    fromList,
    fromMap,
    member,
    bind,
    typeOf,
    members,
    insert,
    delete,
    without,
    unions,
    Apply (..),
    (<:>),
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Pure.Typing.Type (Scheme (..), Type (..))
import Utility.Common (Id)
import Utility.Strings (li, (+\+))

-- ENVIRONMENT -----------------------------------------------------------------

newtype Env a = Env (Map Id a)

type Subst = Env Type

type Context = Env Scheme

-- INSTANCES -------------------------------------------------------------------

instance Functor Env where
  fmap f (Env m) = Env $ Map.map f m

-- CONSTRUCT -------------------------------------------------------------------

fromList :: [(Id, a)] -> Env a
fromList = Env . Map.fromList

fromMap :: Map Id a -> Env a
fromMap = Env

empty :: Env a
empty = Env Map.empty

bind :: Id -> a -> Env a
bind k = Env . Map.singleton k

-- COMBINE ---------------------------------------------------------------------

(<:>) :: Env Type -> Env Type -> Env Type
s1@(Env m1) <:> (Env m2) = Env $ Map.union (Map.map (s1 +->) m2) m1
-- ^ The union is left biased.

unions :: (Foldable t) => t (Env Type) -> Env Type
unions = foldl (<:>) empty

-- UPDATE ----------------------------------------------------------------------

insert :: Id -> a -> Env a -> Env a
insert i a (Env m) = Env $ Map.insert i a m

delete :: Id -> Env a -> Env a
delete i (Env m) = Env $ Map.delete i m

without :: (Foldable t) => t Id -> Env a -> Env a
without is env = foldr delete env is

-- QUERY -----------------------------------------------------------------------

member :: Id -> Env a -> Bool
member k (Env m) = Map.member k m

typeOf :: Id -> Env a -> Maybe a
typeOf i (Env m) = Map.lookup i m

members :: Env a -> Set Id
members (Env m) = Map.keysSet m

-- APPLY -----------------------------------------------------------------------

class Apply a where
  (+->) :: Env Type -> a -> a

instance Apply Type where
  (Env env) +-> (Var var) = fromMaybe (Var var) (Map.lookup var env)
  (Env env) +-> (Rigid var) = fromMaybe (Rigid var) (Map.lookup var env)
  env +-> (arg :-> res) = (+->) env arg :-> (+->) env res
  env +-> (Cons i ps) = Cons i $ map (env +->) ps

instance (Apply t) => Apply [t] where
  env +-> ts = map (env +->) ts

instance Apply Scheme where
  -- The fold takes care of name shadowing.
  (Env env) +-> (vars :. t) = vars :. (Env (foldr Map.delete env vars) +-> t)

instance (Apply a) => Apply (Env a) where
  subst +-> ctx = fmap (subst +->) ctx

-- SHOW ------------------------------------------------------------------------

instance (Show a) => Show (Env a) where
  show (Env m) = "{" +\+ unlines (map showPair $ Map.toList m) ++ "}"
    where
      showPair (k, v) = li k ++ ": " ++ show v