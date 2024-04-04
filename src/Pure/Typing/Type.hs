{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Type
  ( TypeEnv,
    Type (..),
    TypeDef (..),
    Def (..),
    Typed (..),
    TExpr,
    -- TYPE CONSTRUCTORS
    tList,
    tStr,
    tFloat,
    tInt,
    tBool,
    -- INSPECT
    typeOf,
  )
where

import Data.Map.Strict (Map)
import Pure.Expr (Expr)
import qualified Pure.Sacred as S
import Utility.Common (Id)
import Utility.Strings (Parens (..), parenthesised, (+-+))

type TypeEnv = Map Id Type

data Type
  = Type :-> Type -- a -> b
  | Cons Id [Type] -- a b c
  | Poly Id -- a
  deriving (Eq)

data Typed a = a ::= Type deriving (Eq)

newtype TypeDef = Is [(Id, [Id], [Type])] deriving (Eq)
-- ^                  name poly  constructors

data Def = Id := Typed TypedExpr deriving (Eq)

type TExpr = Typed Expr

data TypedExpr
  = Lam Id TExpr
  | If TExpr TExpr TExpr
  | App TExpr [TExpr]
  | List [TExpr]
  | Primitive [TExpr]
  deriving (Eq)

-- TYPE CONSTRUCTORS -----------------------------------------------------------

tList :: [Type] -> Type
tList = Cons "List"

tStr :: Type
tStr = Cons "Str" []

tFloat :: Type
tFloat = Cons "Float" []

tInt :: Type
tInt = Cons "Int" []

tBool :: Type
tBool = Cons "Bool" []

-- INSPECT ---------------------------------------------------------------------

typeOf :: Typed a -> Type
typeOf (_ ::= t) = t

-- SHOW ------------------------------------------------------------------------

instance Parens Type where
  parens t@(Cons _ []) = show t
  parens t = parenthesised $ show t

instance Show Type where
  show (a :-> b) = show a +-+ S.arrow +-+ show b
  show (Cons i ts) = i +-+ unwords (map parens ts)
  show (Poly a) = a
