{-# LANGUAGE FlexibleInstances #-}

module Pure.Expr (Expr (..), Literal (..), positionOf) where

import qualified Pure.Sacred as S
import Text.Parsec (SourcePos)
import Utility.Common (Id)
import Utility.Strings (Parens (..), list, parenthesised, (+-+))

-- EXPRESSION ------------------------------------------------------------------

data Expr
  = Lam Id Expr SourcePos
  | When Expr [(Literal, Expr)] SourcePos
  | If Expr Expr Expr SourcePos
  | App Expr Expr SourcePos
  | Literal Literal
  deriving (Eq)

data Literal
  = List [Expr] SourcePos
  | Id Id SourcePos
  | Str String SourcePos
  | Float Double SourcePos
  | Int Integer SourcePos
  | Bool Bool SourcePos
  deriving (Eq)

-- POSITION --------------------------------------------------------------------

class Position a where
  positionOf :: a -> SourcePos

instance Position Expr where
  positionOf (Lam _ _ pos) = pos
  positionOf (When _ _ pos) = pos
  positionOf (If _ _ _ pos) = pos
  positionOf (App _ _ pos) = pos
  positionOf (Literal lit) = positionOf lit

instance Position Literal where
  positionOf (List _ pos) = pos
  positionOf (Id _ pos) = pos
  positionOf (Str _ pos) = pos
  positionOf (Float _ pos) = pos
  positionOf (Int _ pos) = pos
  positionOf (Bool _ pos) = pos

-- SHOW ------------------------------------------------------------------------

instance Parens Expr where
  parens l@(Literal _) = show l
  parens ex = parenthesised $ show ex

instance Show Expr where
  show (Literal literal) = show literal
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
  show (Bool bool _) = show bool
  show (Int int _) = show int
  show (Float number _) = show number
  show (Str str _) = show str
  show (Id ident _) = ident
  show (List l _) = list (map show l)