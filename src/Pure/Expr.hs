module Pure.Expr (Expr (..), positionOf) where

import qualified Pure.Sacred as S
import Text.Parsec (SourcePos)
import Utility.Common (Id)
import Utility.Strings (Parens (..), list, parenthesised, (+-+))

-- EXPRESSION ------------------------------------------------------------------

data Expr
  = Lam Id Expr SourcePos
  | If Expr Expr Expr SourcePos
  | App Expr Expr SourcePos
  | List [Expr] SourcePos
  | Id Id SourcePos
  | Str String SourcePos
  | Float Double SourcePos
  | Int Integer SourcePos
  | Bool Bool SourcePos
  deriving (Eq)

-- POSITION --------------------------------------------------------------------

positionOf :: Expr -> SourcePos
positionOf (Lam _ _ pos) = pos
positionOf (If _ _ _ pos) = pos
positionOf (App _ _ pos) = pos
positionOf (List _ pos) = pos
positionOf (Id _ pos) = pos
positionOf (Str _ pos) = pos
positionOf (Float _ pos) = pos
positionOf (Int _ pos) = pos
positionOf (Bool _ pos) = pos

-- SHOW ------------------------------------------------------------------------

instance Parens Expr where
  parens b@(Bool _ _) = show b
  parens i@(Int _ _) = show i
  parens f@(Float _ _) = show f
  parens s@(Str _ _) = show s
  parens i@(Id _ _) = show i
  parens l@(List _ _) = show l
  parens ex = parenthesised $ show ex

instance Show Expr where
  show (Bool bool _) = show bool
  show (Int int _) = show int
  show (Float number _) = show number
  show (Str str _) = show str
  show (Id ident _) = ident
  show (List l _) = list (map show l)
  show (App f arg _) = show f +-+ parens arg
  show (If x y z _) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex _) = p +-+ S.arrow +-+ show ex