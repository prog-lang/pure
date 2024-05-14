{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pure.Expr (Expr (..), positionOf) where

import Data.Char (isUpperCase)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Pure.Sacred as S
import Pure.Typing.Free (Free (..))
import Text.Parsec (SourcePos)
import Utility.Common (Id)
import Utility.Strings (Parens (..), parenthesised, (+-+))

-- EXPRESSION ------------------------------------------------------------------

data Expr
  = Lam Id Expr SourcePos
  | XLam Expr Expr SourcePos
  | If Expr Expr Expr SourcePos
  | App Expr Expr SourcePos
  | Id Id SourcePos
  | Str String SourcePos
  | Float Double SourcePos
  | Int Integer SourcePos
  | Bool Bool SourcePos
  -- \| When Expr [(Literal, Expr)] SourcePos
  deriving (Eq)

-- FREE ------------------------------------------------------------------------

instance Free Expr where
  free (Lam i e _) = Set.union (freeVar i) (free e)
  free (XLam e1 e2 _) = Set.unions $ map free [e1, e2]
  free (If e1 e2 e3 _) = Set.unions $ map free [e1, e2, e3]
  free (App e1 e2 _) = Set.unions $ map free [e1, e2]
  free (Id i _) = freeVar i
  free _ = Set.empty

freeVar :: Id -> Set Id
freeVar i@(l : _) =
  if l == S.underscore || isUpperCase l
    then Set.empty
    else Set.singleton i
freeVar "" = undefined

-- POSITION --------------------------------------------------------------------

class Position a where
  positionOf :: a -> SourcePos

instance Position Expr where
  positionOf (Lam _ _ pos) = pos
  positionOf (XLam _ _ pos) = pos
  positionOf (If _ _ _ pos) = pos
  positionOf (App _ _ pos) = pos
  positionOf (Id _ pos) = pos
  positionOf (Str _ pos) = pos
  positionOf (Float _ pos) = pos
  positionOf (Int _ pos) = pos
  positionOf (Bool _ pos) = pos

-- SHOW ------------------------------------------------------------------------

instance Parens Expr where
  parens literal@(Id _ _) = show literal
  parens literal@(Str _ _) = show literal
  parens literal@(Float _ _) = show literal
  parens literal@(Int _ _) = show literal
  parens literal@(Bool _ _) = show literal
  parens ex = parenthesised $ show ex

instance Show Expr where
  show (Id i _) = i
  show (Str str _) = show str
  show (Float f _) = show f
  show (Int int _) = show int
  show (Bool bool _) = show bool
  show (App f arg _) = show f +-+ parens arg
  show (If x y z _) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam from to _) = from +-+ S.arrow +-+ show to
  show (XLam from to _) = parens from +-+ S.arrow +-+ show to

-- show (When _ [] _) = undefined
-- show (When e branches _) =
--   S.when +-+ show e +-+ S.is +-+ unwords (map showBranch branches)
--   where
--     showBranch (pattern, result) =
--       [S.bar] +-+ show pattern +-+ S.then_ +-+ show result
