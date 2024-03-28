{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TypeChecker.TIMonad where

import Control.Monad.State (MonadState (..), gets, modify)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Bifunctor (first, second)
import Data.Functor ((<&>))
import TypeChecker.Id (enumId)
import TypeChecker.Kind (Kind)
import TypeChecker.Pred (Pred (..), Qual (..))
import TypeChecker.Scheme (Scheme (..))
import TypeChecker.Subst (Subst, Types (apply, tv), nullSubst, (@@))
import TypeChecker.Type (Type (TAp, TGen, TVar), Tyvar (..))
import TypeChecker.Unify (Unify (mgu))

newtype TI a
  = TI (StateT (Subst, Int) (Either String) a)
  deriving (Functor, Applicative, Monad, MonadState (Subst, Int))

instance MonadFail TI where
  fail = error -- BOOM -- FIXME

runTI :: TI a -> Either String a
runTI (TI f) = evalStateT f (nullSubst, 0)

getSubst :: TI Subst
getSubst = gets fst

unify :: Type -> Type -> TI ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

trim :: [Tyvar] -> TI ()
trim vs = do
  (s, n) <- get
  let s' = [(v, t) | (v, t) <- s, v `elem` vs]
      force = length (tv (map snd s'))
      _ = force `seq` s'
  put (s', n)

extSubst :: Subst -> TI ()
extSubst s' = modify (first (s' @@))

newTVar :: Kind -> TI Type
newTVar k = do
  (s, n) <- get
  let v = Tyvar (enumId n) k
  put (s, n + 1)
  pure (TVar v)

freshInst :: Scheme -> TI (Qual Type)
freshInst (Forall ks qt) = mapM newTVar ks <&> flip inst qt

class Instantiate t where
  inst :: [Type] -> t -> t

instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n) = ts !! n
  inst _ t = t

instance (Instantiate a) => Instantiate [a] where
  inst ts = map (inst ts)

instance (Instantiate t) => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t

instance Instantiate Pred where
  inst ts (IsIn c t) = IsIn c (inst ts t)
