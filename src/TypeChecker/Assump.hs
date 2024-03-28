module TypeChecker.Assump where

import TypeChecker.Id (Id)
import TypeChecker.PPrint (PPrint (pprint), nest, text, ($$), (<+>))
import TypeChecker.Scheme (Scheme)
import TypeChecker.Subst (Types (..))

data Assump = Id :>: Scheme

instance PPrint Assump where
  pprint (i :>: s) = (text (show i) <+> text ":>:") $$ nest 2 (pprint s)

instance Types Assump where
  apply s (i :>: sc) = i :>: (apply s sc)
  tv (_ :>: sc) = tv sc

find :: (MonadFail m) => Id -> [Assump] -> m Scheme
find i [] = fail ("unbound identifier: " ++ i)
find i ((i' :>: sc) : as) = if i == i' then return sc else find i as
