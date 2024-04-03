{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pure.PreType
  ( Module (..),
    TypeDef (..),
    Definition (..),
    Expression (..),
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Pure.Parser (Expr, TypeHint)
import qualified Pure.Parser as Parser
import Utility.Common (Id)
import Utility.Convert (TryInto (..))
import Utility.Result (Result (..))
import Utility.Strings (commad, (+-+))

-- TYPES -----------------------------------------------------------------------

data Module = Module
  { typeDefs :: [TypeDef],
    definitions :: [Definition],
    exports :: [Id]
  }

newtype TypeDef = Is [(Id, [Id], [TypeHint])]
-- ^                  name poly  constructors

data Definition = Id := Expression

data Expression = Expr ::= TypeHint

-- ERRORS ----------------------------------------------------------------------

typeHintVsDefMismatch :: Map String a1 -> Map String a2 -> String
typeHintVsDefMismatch thm dm =
  if Map.size thm > Map.size dm
    then "these type hints miss their implementations:" +-+ noImpl
    else "these definitions don't have a type hint:" +-+ noTypeHint
  where
    noImpl = curry commadDiff thm dm
    noTypeHint = curry commadDiff dm thm
    commadDiff = commad . Map.keys . uncurry Map.difference

-- CONVERT ---------------------------------------------------------------------

instance TryInto Parser.Module String Module where
  tryInto pm =
    if Map.size thm /= Map.size dm
      then Err $ typeHintVsDefMismatch thm dm
      else Ok $ Module {typeDefs = tds, definitions = ds, exports = exps}
    where
      exps = Parser.exports pm
      ds = makeDefinitions dm thm
      thm = makeTypeHintMap defs
      dm = makeDefMap defs
      tds = collectTypeDefs defs
      defs = Parser.definitions pm

collectTypeDefs :: [Parser.Definition] -> [TypeDef]
collectTypeDefs = mapMaybe unwrapTypeDef
  where
    unwrapTypeDef (Parser.TypeDef i ps ops) = Just $ Is [(i, ps, ops)]
    unwrapTypeDef _ = Nothing

makeDefinitions :: Map Id Expr -> Map Id TypeHint -> [Definition]
makeDefinitions es =
  map (uncurry (:=))
    . Map.toList
    . Map.intersectionWith (::=) es

makeTypeHintMap :: [Parser.Definition] -> Map Id TypeHint
makeTypeHintMap = Map.fromList . mapMaybe unwrapTypeHint
  where
    unwrapTypeHint (Parser.TypeHint i th) = Just (i, th)
    unwrapTypeHint _ = Nothing

makeDefMap :: [Parser.Definition] -> Map Id Expr
makeDefMap = Map.fromList . mapMaybe unwrapDefinition
  where
    unwrapDefinition (Parser.ValueDef i e) = Just (i, e)
    unwrapDefinition _ = Nothing
