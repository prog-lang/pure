module TypeChecker.TIProg where

import TypeChecker.Assump (Assump)
import TypeChecker.Infer ()
import TypeChecker.Pred (ClassEnv, Pred, reduce)
import TypeChecker.Subst (Types (apply, tv), (@@))
import TypeChecker.TIMain (BindGroup, defaultSubst, tiBindGroup, tiSeq)
import TypeChecker.TIMonad (TI, getSubst, runTI, trim)

type Program = [BindGroup]

tiProgram :: ClassEnv -> [Assump] -> Program -> Either String [Assump]
tiProgram ce as bgs = runTI $
  do
    (ps, as') <- tiSeq tiBindGroup ce as bgs
    s <- getSubst
    let rs = reduce ce (apply s ps)
    s' <- defaultSubst ce [] rs
    return (apply (s' @@ s) as')

tiProgram' :: ClassEnv -> [Assump] -> Program -> Either String [Assump]
tiProgram' ce as bgs = runTI $
  do
    (ps, as') <- tiSeq tiBindGroup' ce as bgs
    s <- getSubst
    let rs = reduce ce (apply s ps)
    s' <- defaultSubst ce [] rs
    return (apply (s' @@ s) as')

tiBindGroup' :: ClassEnv -> [Assump] -> BindGroup -> TI ([Pred], [Assump])
tiBindGroup' ce as bs = do
  (ps, as') <- tiBindGroup ce as bs
  trim (tv (as' ++ as))
  return (ps, as')
