{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Infer
  ( Context,
    TI,
    Error (..),
    assert,
    infer,
    runTI,
    evalTI,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError, withError)
import Control.Monad.State (State, evalState, get, put, runState)
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Pure.Expr (Expr (..))
import Pure.Typing.Env (Apply (..), Env (..), (<:>))
import qualified Pure.Typing.Env as Env
import Pure.Typing.Error (Error (..))
import Pure.Typing.Free (Free (..))
import Pure.Typing.Type (Scheme (..), Type (..), tBool, tFloat, tInt, tList, tStr)
import Utility.Common (Id)
import Utility.Fun ((|>))
import Utility.Pretty (printSection)
import Utility.Result (Result (..))
import qualified Utility.Result as Result
import Utility.Strings (base26, (+-+), (+\\+))

-- TYPES -----------------------------------------------------------------------

type Subst = Env Type

type Context = Env Scheme

type TI a = ExceptT Error (State Int) a

-- RUN -------------------------------------------------------------------------

runTI :: TI a -> (Result Error a, Int)
runTI ti = let (s, a) = runState (runExceptT ti) 0 in (Result.fromEither s, a)

evalTI :: TI a -> Result Error a
evalTI ti = evalState (runExceptT ti) 0 |> Result.fromEither

-- ERRORS ----------------------------------------------------------------------

throwUnboundVariableError :: Id -> TI a
throwUnboundVariableError = throwError . UnboundVariableError

throwOccursCheckError :: TI a
throwOccursCheckError = throwError OccursCheckError

throwUnificationError :: Type -> Type -> TI a
throwUnificationError t r = throwError $ UnificationError t r

-- INFER & ASSERT --------------------------------------------------------------

assert :: Context -> Scheme -> Expr -> TI Scheme
assert ctx hint expr = do
  (s1, tyExpr) <- infer ctx expr
  let schemeExpr = generalize ctx (s1 +-> tyExpr)
  let wrap = Stack $ AssertionError hint schemeExpr
  withError wrap $ do assert' ctx hint expr

assert' :: Context -> Scheme -> Expr -> TI Scheme
assert' ctx hint expr = do
  (s1, tyExpr) <- infer ctx expr
  tyHint <- instantiateWithRigidVars hint
  s2 <- unify tyHint tyExpr
  return $ generalize ctx (s2 <:> s1 +-> tyHint)

infer :: Context -> Expr -> TI (Subst, Type)
infer _ (Bool _ _) = return (Env.empty, tBool)
infer _ (Int _ _) = return (Env.empty, tInt)
infer _ (Float _ _) = return (Env.empty, tFloat)
infer _ (Str _ _) = return (Env.empty, tStr)
infer ctx (Id i _) =
  case Env.typeOf i ctx of
    Nothing -> throwUnboundVariableError i
    Just scheme -> instantiate scheme <&> (,) Env.empty
infer _ (List [] _) = var <&> (,) Env.empty
infer ctx (List (x : xs) pos) = do
  (s1, tx) <- infer ctx x
  (s2, txs) <- infer (s1 +-> ctx) (List xs pos)
  s3 <- unify (tList tx) txs
  return (s3 <:> s2 <:> s1, s3 +-> txs)
infer ctx (App fun arg _) = do
  (s1, tyFun) <- infer ctx fun
  (s2, tyArg) <- infer (s1 +-> ctx) arg
  tyRes <- var
  s3 <- unify (tyArg :-> tyRes) (s2 +-> tyFun)
  return (s3 <:> s2 <:> s1, s3 +-> tyRes)
infer ctx (If condition e1 e2 _) = do
  (s1, t1) <- infer ctx condition
  (s2, t2) <- infer (s1 +-> ctx) e1
  (s3, t3) <- infer (s2 <:> s1 +-> ctx) e2
  s4 <- unify (s3 <:> s2 <:> s1 +-> t1) tBool
  s5 <- unify (s4 <:> s3 <:> s2 <:> s1 +-> t2) t3
  let s = s5 <:> s4 <:> s3 <:> s2 <:> s1
  let t = s +-> t2
  return (s, t)
infer ctx (Lam binder body _) = do
  tyBinder <- var
  let tmpCtx = Env.insert binder ([] :. tyBinder) ctx
  (s1, tyBody) <- infer tmpCtx body
  return (s1, (s1 +-> tyBinder) :-> tyBody)

-- HELPERS ---------------------------------------------------------------------

unify :: Type -> Type -> TI Subst
unify (Var u) t = varBind u t
unify t (Var u) = varBind u t
unify t@(Rigid x) r@(Rigid y)
  | x == y = return Env.empty
  | otherwise = throwUnificationError t r
unify t@(Cons i []) r@(Cons i' [])
  | i == i' = return Env.empty
  | otherwise = throwUnificationError t r
unify t@(Cons i (x : xs)) r@(Cons i' (y : ys))
  | i == i' = do
      s <- unify x y
      ss <- unify (Cons i (s +-> xs)) (Cons i (s +-> ys))
      return $ ss <:> s
  | otherwise = throwUnificationError t r
unify (l :-> r) (l' :-> r') = do
  s1 <- unify l l'
  s2 <- unify (s1 +-> r) (s1 +-> r')
  return $ s2 <:> s1
unify t r = throwUnificationError t r

var :: TI Type
var = do
  i <- get
  put $ i + 1
  return $ Var $ base26 i

rigid :: TI Type
rigid = do
  i <- get
  put $ i + 1
  return $ Rigid $ base26 i ++ "*"

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Id -> Type -> TI Subst
varBind v ty
  | ty == Var v = return Env.empty
  | Set.member v (free ty) = throwOccursCheckError
  | otherwise = return $ Env.bind v ty

generalize :: Context -> Type -> Scheme
generalize ctx t = Set.toList (Set.difference (free t) (free ctx)) :. t

instantiate :: Scheme -> TI Type
instantiate (vars :. ty) = do
  newVars <- traverse (const var) vars
  let subst = Env.fromList $ zip vars newVars
  return $ subst +-> ty

instantiateWithRigidVars :: Scheme -> TI Type
instantiateWithRigidVars (vars :. ty) = do
  newVars <- traverse (const rigid) vars
  let subst = Env.fromList $ zip vars newVars
  return $ subst +-> ty

-- REPL TESTING ----------------------------------------------------------------

testAssert :: Scheme -> Expr -> IO ()
testAssert hint expr = do
  case evalTI (assert primitives hint expr) of
    Err err -> printSection "type hint mismatch" $ ":=" +-+ show expr +\\+ show err
    Ok ok -> putStrLn $ "\nOK:" +-+ show ok ++ "\n"

testTI :: Expr -> IO ()
testTI expr = do
  case evalTI (typeInference primitives expr) of
    Err err -> printSection "inference failure" $ ":=" +-+ show expr +\\+ show err
    Ok t -> putStrLn $ "\n" ++ show (generalize Env.empty t) ++ "\n"

typeInference :: Context -> Expr -> TI Type
typeInference ctx expr = infer ctx expr <&> uncurry (+->)

primitives :: Context
primitives =
  Env.fromList
    [ ("id", ["a"] :. Var "a" :-> Var "a"),
      ("always", ["a", "b"] :. Var "a" :-> Var "b" :-> Var "a"),
      ("(+)", [] :. tInt :-> tInt :-> tInt),
      ("(:)", ["a"] :. Var "a" :-> tList (Var "a") :-> tList (Var "a")),
      ("null", ["a"] :. tList (Var "a"))
    ]
