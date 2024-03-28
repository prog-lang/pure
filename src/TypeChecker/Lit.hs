module TypeChecker.Lit (Literal (..), tiLit) where

import TypeChecker.Infer ()
import TypeChecker.Kind (Kind (Star))
import TypeChecker.Pred (Pred (..))
import TypeChecker.TIMonad (TI, newTVar)
import TypeChecker.Type (Type, tChar, tString)

data Literal
  = LitInt Integer
  | LitChar Char
  | LitRat Rational
  | LitStr String

tiLit :: Literal -> TI ([Pred], Type)
tiLit (LitChar _) = return ([], tChar)
tiLit (LitInt _) = newTVar Star >>= \v -> return ([IsIn "Num" [v]], v)
tiLit (LitStr _) = return ([], tString)
tiLit (LitRat _) = newTVar Star >>= \v -> return ([IsIn "Fractional" [v]], v)
