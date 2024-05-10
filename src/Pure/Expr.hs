{-# LANGUAGE FlexibleInstances #-}

module Pure.Expr (Expr (..), Literal (..), positionOf) where

import qualified Pure.Sacred as S
import Text.Parsec (SourcePos)
import Utility.Common (Id)
import Utility.Strings (Parens (..), list, parenthesised, (+-+))

-- EXPRESSION ------------------------------------------------------------------

data Expr
  = Lam Id Expr SourcePos
  | When Expr [(Expr, Expr)] SourcePos
  | If Expr Expr Expr SourcePos
  | App Expr Expr SourcePos
  | Literal Literal SourcePos
  deriving (Eq)

data Literal
  = List [Expr]
  | Id Id
  | Str String
  | Float Double
  | Int Integer
  | Bool Bool
  deriving (Eq)

-- POSITION --------------------------------------------------------------------

positionOf :: Expr -> SourcePos
positionOf (Lam _ _ pos) = pos
positionOf (When _ _ pos) = pos
positionOf (If _ _ _ pos) = pos
positionOf (App _ _ pos) = pos
positionOf (Literal _ pos) = pos

-- SHOW ------------------------------------------------------------------------

instance Parens Expr where
  parens l@(Literal _ _) = show l
  parens ex = parenthesised $ show ex

instance Show Expr where
  show (Literal literal _) = show literal
  show (App f arg _) = show f +-+ parens arg
  show (If x y z _) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex _) = p +-+ S.arrow +-+ show ex
  show (When _ [] _) = undefined
  show (When e branches _) =
    S.when +-+ show e +-+ S.is +-+ unwords (map showBranch branches)
    where
      showBranch (pattern, result) =
        [S.bar] +-+ show pattern +-+ S.then_ +-+ show result

instance Show Literal where
  show (Bool bool) = show bool
  show (Int int) = show int
  show (Float number) = show number
  show (Str str) = show str
  show (Id ident) = ident
  show (List l) = list (map show l)