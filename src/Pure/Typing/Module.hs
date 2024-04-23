module Pure.Typing.Module
  ( Module (..),
    Def (..),
    TypeCons (..),
    contextOf,
    names,
    defs,
    cons,
    exportsExistingNames,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set, isSubsetOf, toList, (\\))
import qualified Data.Set as Set
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

data TypeCons = TypeCons Id Scheme deriving (Eq)

-- TRANSFORM -------------------------------------------------------------------

contextOf :: Module -> Context
contextOf modul =
  Env.fromMap $
    Map.union (typeCons modul) (Map.map snd $ definitions modul)

-- QUERY -----------------------------------------------------------------------

names :: Module -> Set Id
names modul =
  Set.union
    (Map.keysSet $ definitions modul)
    (Map.keysSet $ typeCons modul)

defs :: Module -> [Def]
defs = map repackage . Map.toList . definitions
  where
    repackage (name, (expr, scheme)) = Def name expr scheme

cons :: Module -> [TypeCons]
cons = map (uncurry TypeCons) . Map.toList . typeCons

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
