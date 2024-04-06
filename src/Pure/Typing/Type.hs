{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Type
  ( Module (..),
    Type (..),
    TypeDef (..),
    Def (..),
    Typed (..),
    DExpr (..),
    TExpr,
    -- TYPE CONSTRUCTORS
    tType,
    tList,
    tStr,
    tFloat,
    tInt,
    tBool,
    -- INSPECT
    typeOf,
  )
where

import Pure.Expr (Expr)
import qualified Pure.Sacred as S
import Utility.Common (Id)
import Utility.Strings (Parens (..), parenthesised, (+-+))

data Module = Module
  { typeDefs :: [TypeDef],
    definitions :: [Def],
    exports :: [Id]
  }

data Type
  = Type :-> Type -- a -> b
  | Cons Id [Type] -- a b c
  | Poly Id -- a
  deriving (Eq)

data Typed a = a ::= Type deriving (Eq)

data TypeDef = Is Id [Id] [Type] deriving (Eq)
-- ^            name poly  constructors

data Def = Id := TExpr deriving (Eq)

type TExpr = Typed DExpr

-- | DExpr stands for "decorated" expression.
data DExpr
  = Lam Id TExpr
  | If TExpr TExpr TExpr
  | App TExpr [TExpr]
  | List [TExpr]
  | Lit Expr
  deriving (Eq)

-- TYPE CONSTRUCTORS -----------------------------------------------------------

tType :: Type
tType = Cons "Type" []

tList :: Type -> Type
tList a = Cons "List" [a]

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
