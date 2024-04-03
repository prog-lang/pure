module Pure.TypeSystem.Unify (Unify (..), Match (..)) where

import Control.Monad (foldM, zipWithM)
import Pure.TypeSystem.Base (HasKind (kind), Type (TApp, TCon, TVar), TypeVar)
import Pure.TypeSystem.Subst (Subst, Typed (..), conserve, merge, (+->), (@@))

--- UNIFY ----------------------------------------------------------------------

class Unify t where
  mgu :: (MonadFail m) => t -> t -> m Subst

instance Unify Type where
  mgu (TApp l r) (TApp l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s2 @@ s1)
  mgu (TVar u) t = bind u t
  mgu t (TVar u) = bind u t
  mgu (TCon tc1) (TCon tc2)
    | tc1 == tc2 = return conserve
  mgu _ _ = fail "types do not unify"

instance (Unify t, Typed t) => Unify [t] where
  mgu (x : xs) (y : ys) = do
    s1 <- mgu x y
    s2 <- mgu (apply s1 xs) (apply s1 ys)
    return (s2 @@ s1)
  mgu [] [] = return conserve
  mgu _ _ = fail "lists do not unify"

bind :: (MonadFail m) => TypeVar -> Type -> m Subst
bind u t
  | t == TVar u = return conserve
  | u `elem` typeVar t = fail "occurs check fails"
  | kind u /= kind t = fail "kinds do not match"
  | otherwise = return (u +-> t)

--- MARCH ----------------------------------------------------------------------

class Match t where
  match :: (MonadFail m) => t -> t -> m Subst

instance Match Type where
  match (TApp l r) (TApp l' r') = do
    sl <- match l l'
    sr <- match r r'
    merge sl sr
  match (TVar u) t | kind u == kind t = return (u +-> t)
  match (TCon tc1) (TCon tc2) | tc1 == tc2 = return conserve
  match _ _ = fail "types do not match"

instance (Match t) => Match [t] where
  match ts ts' = zipWithM match ts ts' >>= foldM merge conserve
