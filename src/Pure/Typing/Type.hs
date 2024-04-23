{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Type
  ( Type (..),
    Scheme (..),
    typeVars,
    final,
    called,
    numberOfParams,
    tType,
    tList,
    tStr,
    tFloat,
    tInt,
    tBool,
  )
where

import qualified Pure.Sacred as S
import Utility.Common (Id)
import Utility.Strings (Parens (..), commad, parenthesised, (+-+))

-- TYPE & SCHEME ---------------------------------------------------------------

infixr 5 :->

infixr 5 :.

data Type
  = Type :-> Type -- a -> b
  | Cons Id [Type] -- a b c
  | Var Id -- a
  | Rigid Id -- a*
  deriving (Eq, Ord)

-- SCHEME ----------------------------------------------------------------------

data Scheme = [Id] :. Type deriving (Eq, Ord)

typeVars :: Scheme -> [Id]
typeVars (vs :. _) = vs

-- INSPECT ---------------------------------------------------------------------

final :: Type -> Type
final (_ :-> ty) = final ty
final ty = ty

called :: Type -> String
called (_ :-> _) = "Arrow"
called (Cons name _) = name
called (Var name) = name
called (Rigid name) = name

numberOfParams :: Type -> Int
numberOfParams (_ :-> ty) = 1 + numberOfParams ty
numberOfParams _ = 0

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

-- SHOW ------------------------------------------------------------------------

instance Parens Type where
  parens t@(Cons _ []) = show t
  parens t@(Var _) = show t
  parens t@(Rigid _) = show t
  parens t = parenthesised $ show t

instance Show Type where
  show (a :-> b) = show a +-+ S.arrow +-+ show b
  show (Cons i []) = i
  show (Cons i ts) = i +-+ unwords (map parens ts)
  show (Var i) = i
  show (Rigid i) = i

instance Show Scheme where
  show ([] :. t) = show t
  show (vs :. t) = "forall" +-+ commad vs ++ "." +-+ show t