module Pure.Typing.Module
  ( Module (..),
    Def (..),
    checkDef,
  )
where

import Pure.Expr (Expr)
import Pure.Typing.Infer (Context, Error (..), assert, evalTI)
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)
import Utility.Result (Result)

-- MODULE ----------------------------------------------------------------------

data Module = Module
  { definitions :: [Def],
    exports :: [Id]
  }

data Def = Def Id Scheme Expr deriving (Eq)

-- CHECKS ----------------------------------------------------------------------

checkDef :: Context -> Def -> Result Error Scheme
checkDef ctx (Def name hint expr) = evalTI $ assert ctx hint expr
