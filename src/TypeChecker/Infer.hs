module TypeChecker.Infer (Infer) where

import TypeChecker.Assump (Assump)
import TypeChecker.Pred (ClassEnv, Pred)
import TypeChecker.TIMonad (TI)

type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)
