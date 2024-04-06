{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pure.Typing.Valid (validate) where

import Pure.Expr (Expr)
import qualified Pure.Expr as E
import qualified Pure.PreType as Pre
import Pure.Typing.Env (TypeEnv)
import qualified Pure.Typing.Env as TE
import Pure.Typing.Type
  ( DExpr (..),
    Def (..),
    Module (..),
    TExpr,
    Type (..),
    TypeDef (..),
    Typed (..),
    tBool,
    tFloat,
    tInt,
    tList,
    tStr,
    tType,
  )
import qualified Pure.Typing.Type as Type
import Utility.Common (Id)
import Utility.Fun ((|>))
import Utility.Result (Result (..))
import Utility.Strings ((+-+))

-- TYPES -----------------------------------------------------------------------

type Error = String

type V a ok = TypeEnv -> a -> Result Error ok

type I a = V a a

class Validate a ok where
  validate :: V a ok

-- BASICS ----------------------------------------------------------------------

instance Validate Expr TExpr where
  validate _ b@(E.Bool _) = Ok $ Lit b ::= tBool
  validate _ i@(E.Int _) = Ok $ Lit i ::= tInt
  validate _ f@(E.Float _) = Ok $ Lit f ::= tFloat
  validate _ s@(E.Str _) = Ok $ Lit s ::= tStr
  validate env i@(E.Id name) =
    case TE.typeOf name env of
      Just t -> Ok $ Lit i ::= t
      Nothing -> Err $ "unknown identifier:" +-+ name
  validate env (E.List xs) = validate env xs

instance Validate [Expr] (Typed DExpr) where
  validate _ [] = Ok $ List [] ::= tList (Poly "a")
  validate env xs =
    case mapM (validate env :: Expr -> Result Error TExpr) xs of
      Err err -> Err err
      Ok txs -> validate env txs

instance Validate [TExpr] TExpr where
  validate _ txs =
    if allTheSame types
      then Ok $ List txs ::= tList (head types)
      else Err "list values differ by type"
    where
      types = map Type.typeOf txs
      allTheSame s = all (== head s) (tail s)

-- VALIDATE --------------------------------------------------------------------

-- validate :: Pre.Module -> Result Error Module
-- validate = undefined

instance Validate Pre.Module Module where
  validate = undefined

instance Validate TypeDef TypeDef where
  validate env tydef@(Is name ps ts) =
    (validate :: I Id) env name >> return tydef

instance Validate Id Id where
  validate env name =
    if TE.member name env
      then Err $ "redeclared identifier:" +-+ name
      else Ok name
