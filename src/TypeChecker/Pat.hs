module TypeChecker.Pat where

import TypeChecker.Assump (Assump (..))
import TypeChecker.Id (Id)
import TypeChecker.Infer ()
import TypeChecker.Kind (Kind (Star))
import TypeChecker.Lit (Literal, tiLit)
import TypeChecker.Pred (Pred (..), Qual ((:=>)))
import TypeChecker.Scheme (toScheme)
import TypeChecker.TIMonad (TI, freshInst, newTVar, unify)
import TypeChecker.Type (Type, fn)

data Pat
  = PVar Id
  | PWildcard
  | PAs Id Pat
  | PLit Literal
  | PNpk Id Integer
  | PCon Assump [Pat]
  | PLazy Pat

tiPat :: Pat -> TI ([Pred], [Assump], Type)
tiPat (PVar i) = do
  v <- newTVar Star
  return ([], [i :>: toScheme v], v)
tiPat PWildcard = do
  v <- newTVar Star
  return ([], [], v)
tiPat (PAs i pat) = do
  (ps, as, t) <- tiPat pat
  return (ps, (i :>: toScheme t) : as, t)
tiPat (PLit l) = do
  (ps, t) <- tiLit l
  return (ps, [], t)
tiPat (PNpk i k) = do
  t <- newTVar Star
  return ([IsIn "Integral" [t]], [i :>: toScheme t], t)
tiPat (PCon (i :>: sc) pats) = do
  (ps, as, ts) <- tiPats pats
  t' <- newTVar Star
  (qs :=> t) <- freshInst sc
  unify t (foldr fn t' ts)
  return (ps ++ qs, as, t')
tiPat (PLazy pat) = tiPat pat

tiPats :: [Pat] -> TI ([Pred], [Assump], [Type])
tiPats pats = do
  psasts <- mapM tiPat pats
  let ps = concat [ps' | (ps', _, _) <- psasts]
      as = concat [as' | (_, as', _) <- psasts]
      ts = [t | (_, _, t) <- psasts]
  return (ps, as, ts)
