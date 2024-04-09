{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pure.Typing.Prep (prepare) where

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Pure.Expr (Expr)
import qualified Pure.Parser as Parser
import Pure.Typing.Free (Free (..))
import Pure.Typing.Module (Module (..))
import Pure.Typing.Type (Scheme (..))
import Utility.Common (Id)
import Utility.Result (Result (..))
import Utility.Strings (commad, (+-+))

-- ERRORS ----------------------------------------------------------------------

typeHintVsDefMismatch :: Map String a -> Map String b -> String
typeHintVsDefMismatch thm dm =
  if Map.size thm > Map.size dm
    then "these type hints miss their implementations:" +-+ noImpl
    else "these definitions don't have a type hint:" +-+ noTypeHint
  where
    noImpl = curry commadDiff thm dm
    noTypeHint = curry commadDiff dm thm
    commadDiff = commad . Map.keys . uncurry Map.difference

-- CONVERT ---------------------------------------------------------------------

prepare :: Parser.Module -> Result String Module
prepare pm =
  if Map.size thm /= Map.size dm
    then Err $ typeHintVsDefMismatch thm dm
    else Ok $ Module {definitions = ds, exports = exps}
  where
    exps = Set.fromList $ Parser.exports pm
    ds = makeDefinitions dm thm
    thm = makeTypeHintMap defs
    dm = makeDefMap defs
    --   tds = collectTypeDefs defs
    defs = Parser.definitions pm

-- collectTypeDefs :: [Parser.Def] -> [TypeDef]
-- collectTypeDefs = mapMaybe unwrapTypeDef
--   where
--     unwrapTypeDef (Parser.TypeDef i ps ops) = Just $ Is i ps ops
--     unwrapTypeDef _ = Nothing

makeDefinitions :: Map Id Expr -> Map Id Scheme -> Map Id (Expr, Scheme)
makeDefinitions = Map.intersectionWith (,)

makeTypeHintMap :: [Parser.Def] -> Map Id Scheme
makeTypeHintMap = Map.fromList . mapMaybe unwrapTypeHint
  where
    unwrapTypeHint (Parser.TypeHint i th) = Just (i, scheme th)
    unwrapTypeHint _ = Nothing
    scheme t = toList (free t) :. t

makeDefMap :: [Parser.Def] -> Map Id Expr
makeDefMap = Map.fromList . mapMaybe unwrapDefinition
  where
    unwrapDefinition (Parser.ValueDef i e) = Just (i, e)
    unwrapDefinition _ = Nothing
