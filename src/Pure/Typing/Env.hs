{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pure.Typing.Env
  ( Env,
    empty,
    member,
    bind,
    Apply (..),
    (<:>),
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Pure.Typing.Type (Scheme (..), Type (..))
import Utility.Common (Id)

-- ENVIRONMENT -----------------------------------------------------------------

type Env = Map Id Type

class Apply a where
  (+->) :: Env -> a -> a

empty :: Env
empty = Map.empty

bind :: Id -> Type -> Env
bind = Map.singleton

member :: Id -> Env -> Bool
member = Map.member

instance Apply Type where
  env +-> (Var var) = fromMaybe (Var var) (Map.lookup var env)
  env +-> (arg :-> res) = (+->) env arg :-> (+->) env res
  env +-> (Cons i ps) = Cons i $ map (env +->) ps

instance (Apply t) => Apply [t] where
  env +-> ts = map (env +->) ts

instance Apply Scheme where
  -- The fold takes care of name shadowing.
  env +-> (vars :. t) = vars :. (foldr Map.delete env vars +-> t)

(<:>) :: Env -> Env -> Env
s1 <:> s2 = Map.union (Map.map (s1 +->) s2) s1
-- ^ The union is left biased.
