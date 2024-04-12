module Pure.Typing.Module
  ( Module (..),
    Def (..),
    contextOf,
    names,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Pure.Expr (Expr)
import Pure.Typing.Env (Context)
import qualified Pure.Typing.Env as Env
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)

-- MODULE ----------------------------------------------------------------------

data Module = Module
  { definitions :: Map Id (Expr, Scheme),
    exports :: Set Id
  }

data Def = Def Id Expr Scheme deriving (Eq)

-- TRANSFORM -------------------------------------------------------------------

contextOf :: Module -> Context
contextOf = Env.fromMap . Map.map snd . definitions

-- QUERY -----------------------------------------------------------------------

names :: Module -> Set Id
names = Map.keysSet . definitions
