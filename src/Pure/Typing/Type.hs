{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Type
  ( Type (..),
    Typed (..),
    Scheme (..),
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

import qualified Pure.Sacred as S
import Utility.Common (Id)
import Utility.Strings (Parens (..), commad, parenthesised, (+-+))

infixr 5 :->

data Type
  = Type :-> Type -- a -> b
  | Cons Id [Type] -- a b c
  | Var Id -- a
  deriving (Eq, Ord)

data Typed a = a ::= Type deriving (Eq, Ord)

infixr 5 :.

data Scheme = [Id] :. Type

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
  show (Cons i []) = i
  show (Cons i ts) = i +-+ unwords (map parens ts)
  show (Var i) = i

instance Show Scheme where
  show ([] :. t) = show t
  show (vs :. t) = "forall" +-+ commad vs ++ "." +-+ show t