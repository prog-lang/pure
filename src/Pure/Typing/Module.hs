module Pure.Typing.Module
  ( Module (..),
    Def (..),
    contextOf,
    names,
    defs,
    exportsExistingNames,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, isSubsetOf, toList, (\\))
import Pure.Expr (Expr)
import Pure.Typing.Env (Context)
import qualified Pure.Typing.Env as Env
import Pure.Typing.Error (Error (..))
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)
import Utility.Result (Result (..))

-- MODULE ----------------------------------------------------------------------

data Module = Module
  { typeDefs :: Map Id ([Id], [Scheme]),
    typeCons :: Map Id Scheme,
    definitions :: Map Id (Expr, Scheme),
    exports :: Set Id
  }

data Def = Def Id Expr Scheme deriving (Eq)

-- TRANSFORM -------------------------------------------------------------------

contextOf :: Module -> Context
contextOf modul =
  Env.fromMap $
    Map.union (typeCons modul) (Map.map snd $ definitions modul)

-- QUERY -----------------------------------------------------------------------

names :: Module -> Set Id
names = Map.keysSet . definitions

defs :: Module -> [Def]
defs = map repackage . Map.toList . definitions
  where
    repackage (name, (expr, scheme)) = Def name expr scheme

-- CHECK -----------------------------------------------------------------------

exportsExistingNames :: Module -> Result [Error] Module
exportsExistingNames modul =
  if es `isSubsetOf` ns
    then Ok modul
    else Err $ map ModuleExportsUndefinedIdentifier diff
  where
    es = exports modul
    ns = names modul
    diff = toList $ es \\ ns
