module Pure.Typing.Module
  ( Module (..),
    Def (..),
  )
where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Pure.Expr (Expr)
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)

-- MODULE ----------------------------------------------------------------------

data Module = Module
  { definitions :: Map Id (Expr, Scheme),
    exports :: Set Id
  }

data Def = Def Id Expr Scheme deriving (Eq)

-- ERROR -----------------------------------------------------------------------
