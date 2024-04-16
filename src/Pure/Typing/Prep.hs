{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pure.Typing.Prep (prepare) where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set ((\\))
import qualified Data.Set as Set
import Pure.Expr (Expr)
import qualified Pure.Parser as Parser
import Pure.Typing.Free (Free (..))
import Pure.Typing.Module (Module (..))
import qualified Pure.Typing.Module as Module
import Pure.Typing.Type (Scheme (..), Type, typeVars)
import Utility.Common (Id)
import Utility.Result (Result (..))
import Utility.Strings (commad, (+-+))

-- ERRORS ----------------------------------------------------------------------

type Error = String

-- CHECKS ----------------------------------------------------------------------

typeHintVsDefMismatch :: Map Id a -> Map Id b -> Id
typeHintVsDefMismatch thm dm =
  if Map.size thm > Map.size dm
    then "These type hints miss their implementations:" +-+ noImpl
    else "These definitions don't have a type hint:" +-+ noTypeHint
  where
    noImpl = curry commadDiff thm dm
    noTypeHint = curry commadDiff dm thm
    commadDiff = commad . Map.keys . uncurry Map.difference

checkTypeDefs :: Map Id ([Id], [Scheme]) -> [Error]
checkTypeDefs m =
  mapMaybe typeDefHasUnboundVars ds ++ mapMaybe typeDefHasUnusedVars ds
  where
    ds = Map.toList m

typeDefHasUnboundVars :: (Id, ([Id], [Scheme])) -> Maybe Error
typeDefHasUnboundVars (name, (vs, schemes)) =
  if length ftvs > length svs
    then
      Just $
        "Type definition" +-+ name +-+ "has unbound type variables:" +-+ unbound
    else Nothing
  where
    svs = Set.fromList vs
    ftvs = Set.fromList $ concatMap typeVars schemes
    unbound = commad $ toList $ ftvs \\ svs

typeDefHasUnusedVars :: (Id, ([Id], [Scheme])) -> Maybe Error
typeDefHasUnusedVars (name, (vs, schemes)) =
  if length ftvs < length svs
    then
      Just $
        "Type definition" +-+ name +-+ "has unused type variables:" +-+ unused
    else Nothing
  where
    svs = Set.fromList vs
    ftvs = Set.fromList $ concatMap typeVars schemes
    unused = commad $ toList $ svs \\ ftvs

-- CONVERT ---------------------------------------------------------------------

prepare :: Parser.Module -> Result Error Module
prepare pm
  | Map.size thm /= Map.size dm = Err $ typeHintVsDefMismatch thm dm
  | not (null typeDefErrors) = Err $ intercalate "\n\n" typeDefErrors
  | otherwise =
      Module.exportsExistingNames $
        Module
          { typeDefs = tds,
            definitions = ds,
            exports = exps
          }
  where
    exps = Set.fromList $ Parser.exports pm
    typeDefErrors = checkTypeDefs tds
    tds = collectTypeDefs defs
    ds = makeDefinitions dm thm
    thm = makeTypeHintMap defs
    dm = makeDefMap defs
    defs = Parser.definitions pm
    makeDefinitions = Map.intersectionWith (,)

collectTypeDefs :: [Parser.Def] -> Map Id ([Id], [Scheme])
collectTypeDefs = Map.fromList . mapMaybe unwrapTypeDef
  where
    unwrapTypeDef (Parser.TypeDef i ps ops) = Just (i, (ps, map scheme ops))
    unwrapTypeDef _ = Nothing

makeTypeHintMap :: [Parser.Def] -> Map Id Scheme
makeTypeHintMap = Map.fromList . mapMaybe unwrapTypeHint
  where
    unwrapTypeHint (Parser.TypeHint i th) = Just (i, scheme th)
    unwrapTypeHint _ = Nothing

makeDefMap :: [Parser.Def] -> Map Id Expr
makeDefMap = Map.fromList . mapMaybe unwrapDefinition
  where
    unwrapDefinition (Parser.ValueDef i e) = Just (i, e)
    unwrapDefinition _ = Nothing

-- HELPERS ---------------------------------------------------------------------

scheme :: Type -> Scheme
scheme t = toList (free t) :. t