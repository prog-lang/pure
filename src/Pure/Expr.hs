module Pure.Expr (Expr (..)) where

import qualified Pure.Sacred as S
import Utility.Common (Id)
import Utility.Strings (Parens (..), list, parenthesised, (+-+))

-- EXPRESSION ------------------------------------------------------------------

data Expr
  = Lam Id Expr
  | If Expr Expr Expr
  | App Expr [Expr]
  | List [Expr]
  | Id Id
  | Str String
  | Float Double
  | Int Integer
  | Bool Bool
  deriving (Eq)

-- SHOW ------------------------------------------------------------------------

instance Parens Expr where
  parens i@(Int _) = show i
  parens f@(Float _) = show f
  parens s@(Str _) = show s
  parens i@(Id _) = show i
  parens l@(List _) = show l
  parens ex = parenthesised $ show ex

instance Show Expr where
  show (Bool bool) = show bool
  show (Int int) = show int
  show (Float number) = show number
  show (Str str) = show str
  show (Id ident) = ident
  show (List l) = list (map show l)
  show (App ex exs) = unwords $ map parens (ex : exs)
  show (If x y z) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex) = p +-+ S.arrow +-+ show ex