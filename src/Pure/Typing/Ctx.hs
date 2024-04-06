module Pure.Typing.Ctx (Ctx (..), insert, typeOf, empty) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pure.Typing.Env (Apply (..))
import Utility.Common (Id)
import Utility.Fun ((|>))

newtype Ctx a = Ctx (Map Id a)

instance Functor Ctx where
  fmap f (Ctx ctx) = ctx |> Map.map f |> Ctx

instance (Apply a) => Apply (Ctx a) where
  subst +-> ctx = fmap (subst +->) ctx

empty :: Ctx as
empty = Ctx Map.empty

insert :: Id -> a -> Ctx a -> Ctx a
insert i a (Ctx ctx) = Map.insert i a ctx |> Ctx

typeOf :: Id -> Ctx a -> Maybe a
typeOf i (Ctx ctx) = Map.lookup i ctx
