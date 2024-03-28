module TypeChecker.Unify where

import Control.Monad (foldM, zipWithM)
import TypeChecker.Kind ()
import TypeChecker.Subst (Subst, Types (..), merge, nullSubst, (+->), (@@))
import TypeChecker.Type (HasKind (kind), Type (TAp, TCon, TVar), Tyvar)

class Unify t where
  mgu :: (MonadFail m) => t -> t -> m Subst

instance Unify Type where
  mgu (TAp l r) (TAp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 @@ s1)
  mgu (TVar u) t = varBind u t
  mgu t (TVar u) = varBind u t
  mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
  mgu _ _ = fail "types do not unify"

instance (Unify t, Types t) => Unify [t] where
  mgu (x : xs) (y : ys) = do
    s1 <- mgu x y
    s2 <- mgu (apply s1 xs) (apply s1 ys)
    return (s2 @@ s1)
  mgu [] [] = return nullSubst
  mgu _ _ = fail "lists do not unify"

varBind :: (MonadFail m) => Tyvar -> Type -> m Subst
varBind u t
  | t == TVar u = return nullSubst
  | u `elem` tv t = fail "occurs check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return (u +-> t)

class Match t where
  match :: (MonadFail m) => t -> t -> m Subst

instance Match Type where
  match (TAp l r) (TAp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
  match (TVar u) t | kind u == kind t = return (u +-> t)
  match (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return nullSubst
  match _ _ = fail "types do not match"

instance (Match t) => Match [t] where
  match ts ts' = zipWithM match ts ts' >>= foldM merge nullSubst
