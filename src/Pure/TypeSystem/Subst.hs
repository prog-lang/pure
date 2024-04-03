module Pure.TypeSystem.Subst
  ( Subst,
    conserve,
    (+->),
    Typed (..),
    (@@),
    merge,
  )
where

import Data.List (intersect, nub, union)
import Data.Maybe (fromMaybe)
import Pure.TypeSystem.Base (Type (TApp, TVar), TypeVar)

--- SUBSTITUTION ---------------------------------------------------------------

type Subst = [(TypeVar, Type)]

conserve :: Subst
conserve = []

(+->) :: TypeVar -> Type -> Subst
u +-> t = [(u, t)]

infixr 4 @@

(@@) :: Subst -> Subst -> Subst
s1 @@ s2 = [(u, apply s1 t) | (u, t) <- s2] ++ s1

merge :: (MonadFail m) => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1 ++ s2) else fail "merge fails"
  where
    agree = flip all intersectingTypeVars matchAfterSubst
    matchAfterSubst v = apply s1 (TVar v) == apply s2 (TVar v)
    intersectingTypeVars = map fst s1 `intersect` map fst s2

--- TYPED ----------------------------------------------------------------------

class Typed t where
  apply :: Subst -> t -> t
  typeVar :: t -> [TypeVar]

instance Typed Type where
  apply s (TVar u) = fromMaybe (TVar u) (lookup u s)
  apply s (TApp l r) = TApp (apply s l) (apply s r)
  apply _ t = t

  typeVar (TVar u) = [u]
  typeVar (TApp l r) = typeVar l `union` typeVar r
  typeVar _ = []

instance (Typed a) => Typed [a] where
  apply s = map (apply s)
  typeVar = nub . concatMap typeVar
