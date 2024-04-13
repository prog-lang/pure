module Pure.Typing.Module
  ( Module (..),
    Def (..),
    contextOf,
    names,
    defs,
    exportsExistingNames,
    entrypointPresent,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, isSubsetOf, toList, (\\))
import Pure.Expr (Expr)
import Pure.Typing.Env (Context)
import qualified Pure.Typing.Env as Env
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)
import Utility.Result (Result (..))
import Utility.Strings (commad, (+-+))

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

defs :: Module -> [Def]
defs = map repackage . Map.toList . definitions
  where
    repackage (name, (expr, scheme)) = Def name expr scheme

-- CHECK -----------------------------------------------------------------------

type Error = String

exportsExistingNames :: Module -> Result Error Module
exportsExistingNames modul =
  if es `isSubsetOf` ns
    then Ok modul
    else Err $ "Module exports undefined identifiers:" +-+ diff
  where
    es = exports modul
    ns = names modul
    diff = commad $ toList $ es \\ ns

entrypointPresent :: Module -> Result Error Module
entrypointPresent modul =
  if elem entrypoint $ names modul
    then Ok modul
    else Err err
  where
    err = "Entrypoint missing: " ++ entrypoint
    entrypoint = "main"