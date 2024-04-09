module Pure.Typing.Check (typing, check, checkDef) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Pure.Parser as Parser
import qualified Pure.Typing.Env as Env
import Pure.Typing.Error (Error (..))
import Pure.Typing.Infer (Context, assert, evalTI)
import Pure.Typing.Module (Def (..), Module (..))
import Pure.Typing.Prep (prepare)
import Pure.Typing.Type (Scheme (..))
import Utility.Fun ((!>))
import Utility.Result (Result (..), collect, mapErr, (<!>))

-- CHECKS ----------------------------------------------------------------------

typing :: Parser.Module -> Result [Error] Module
typing =
  prepare
    !> mapErr (PreparationError !> List.singleton)
    !> (>>= check Env.empty)

check :: Context -> Module -> Result [Error] Module
check ctx modul = fmap (const modul) $ collect $ map (checkDef ctx) $ defs modul

checkDef :: Context -> Def -> Result Error Scheme
checkDef ctx (Def name expr hint) = result <!> InferenceError name
  where
    result = evalTI $ assert ctx hint expr

-- HELPERS ---------------------------------------------------------------------

defs :: Module -> [Def]
defs = map repackage . Map.toList . definitions
  where
    repackage (name, (expr, scheme)) = Def name expr scheme