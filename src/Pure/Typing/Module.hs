module Pure.Typing.Module
  ( Module (..),
    Def (..),
    check,
    checkDef,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Pure.Expr (Expr)
import Pure.Typing.Infer (Context, Error (..), assert, evalTI)
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)
import Utility.Result (Result (..), collect)

-- MODULE ----------------------------------------------------------------------

data Module = Module
  { definitions :: Map Id (Expr, Scheme),
    exports :: Set Id
  }

data Def = Def Id Expr Scheme deriving (Eq)

defs :: Module -> [Def]
defs = map repackage . Map.toList . definitions
  where
    repackage (name, (expr, scheme)) = Def name expr scheme

-- CHECKS ----------------------------------------------------------------------

check :: Context -> Module -> Result [Error] Module
check ctx modul = fmap (const modul) $ collect $ map (checkDef ctx) $ defs modul

checkDef :: Context -> Def -> Result Error Scheme
checkDef ctx (Def name expr hint) = evalTI $ assert ctx hint expr
