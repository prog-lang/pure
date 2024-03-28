module TypeChecker.Static
  ( module TypeChecker.Id,
    module TypeChecker.Kind,
    module TypeChecker.Type,
    module TypeChecker.Subst,
    module TypeChecker.Pred,
    module TypeChecker.Scheme,
    module TypeChecker.Assump,
    module TypeChecker.TIMonad,
    isIn1,
    mkInst,
    instances,
  )
where

import TypeChecker.Assump
import TypeChecker.Id
import TypeChecker.Kind
import TypeChecker.Pred
import TypeChecker.Scheme
import TypeChecker.Subst
import TypeChecker.TIMonad
import TypeChecker.Type

-----------------------------------------------------------------------------
-- This module contains definitions that do not appear in the
-- typeset version of the paper.

-- We introduce the following definition because it will allow a smoother
-- transition to multiple parameter classes later on ...

isIn1 :: Id -> Type -> Pred
isIn1 i t = IsIn i [t]

mkInst :: (Instantiate a) => [Kind] -> a -> a
mkInst ks = inst ts
  where
    ts = zipWith (\v k -> TVar (Tyvar v k)) vars ks
    vars =
      [[c] | c <- ['a' .. 'z']]
        ++ [c : show n | n <- [0 :: Int ..], c <- ['a' .. 'z']]

instances :: [Inst] -> EnvTransformer
instances = foldr1 (<:>) . map (\(ps :=> p) -> addInst ps p)
