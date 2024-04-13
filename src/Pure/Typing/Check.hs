module Pure.Typing.Check (typing, check, checkDef) where

import Control.Monad.Error.Class (withError)
import qualified Data.List as List
import Pure.Expr (positionOf)
import qualified Pure.Parser as Parser
import Pure.Typing.Error (Error (..))
import Pure.Typing.Infer (Context, assert, evalTI)
import Pure.Typing.Module (Def (..), Module (..), contextOf, defs)
import Pure.Typing.Prep (prepare)
import Pure.Typing.Type (Scheme (..))
import Utility.Fun ((!>))
import Utility.Result (Result (..), collect, mapErr)

-- CHECKS ----------------------------------------------------------------------

typing :: Parser.Module -> Result [Error] Module
typing =
  prepare
    !> mapErr (PreparationError !> List.singleton)
    !> (>>= check)

check :: Module -> Result [Error] Module
check modul = fmap (const modul) $ collect $ map (checkDef ctx) $ defs modul
  where
    ctx = contextOf modul

checkDef :: Context -> Def -> Result Error Scheme
checkDef ctx (Def name expr hint) =
  evalTI $
    withError (At (positionOf expr) . WithId name) $
      do assert ctx hint expr
