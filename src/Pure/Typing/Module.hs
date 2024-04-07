module Pure.Typing.Module (checkDef) where

import Pure.Expr (Expr)
import Pure.Typing.Infer (Context, Error, assert, evalTI)
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)
import Utility.Result (Result)

checkDef :: Context -> Id -> Scheme -> Expr -> Result Error Scheme
checkDef ctx name hint expr = evalTI $ assert ctx hint expr
