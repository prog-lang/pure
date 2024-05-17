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

import Control.Monad (mapAndUnzipM)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withError)
import Control.Monad.State (State, evalState, get, put, runState)
import Data.Functor ((<&>))
import Data.Set ((\\))
import qualified Data.Set as Set
import Pure.Expr (Expr (..))
import Pure.Typing.Env (Apply (..), Context, Subst, (<:>))
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

-- TYPE INFERENCE MONAD --------------------------------------------------------

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
infer ctx (App fun arg _) = do
  (s1, tyFun) <- infer ctx fun
  (s2, tyArg) <- infer (s1 +-> ctx) arg
  tyResult <- var
  s3 <- unify (tyArg :-> tyResult) (s2 +-> tyFun)
  return (s3 <:> s2 <:> s1, s3 +-> tyResult)
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
  let ctx1 = Env.insert binder ([] :. tyBinder) ctx
  (s, tyBody) <- infer ctx1 body
  return (s, (s +-> tyBinder) :-> tyBody)
infer ctx (XLam pattern body _) = do
  let fvs = Set.toList $ free pattern \\ Env.members ctx
  vts <- mapM (const var) fvs <&> zip fvs
  let ctx1 = foldl (\ctx' (v, t) -> Env.insert v ([] :. t) ctx') ctx vts
  (s1, tyPat) <- infer ctx1 pattern
  (s2, tyBody) <- infer (s1 +-> ctx1) body
  let s = Env.without fvs (s2 <:> s1)
  -- !    ^^^^^^^^^^^^^^^
  -- ? Deleting these because patterns of the same `when` expression may
  -- ? redeclare variables.
  return (s, (s +-> tyPat) :-> tyBody)
infer ctx (When x opts _) = do
  (s1, tyCorpse) <- infer ctx x
  (s2, tyOpts) <- mapAndUnzipM (infer $ s1 +-> ctx) opts
  tyResult <- var
  s3 <- unifyList $ (tyCorpse :-> tyResult) : tyOpts
  let s = s3 <:> Env.unions s2 <:> s1
  return (s, s +-> tyResult)

-- HELPERS ---------------------------------------------------------------------

unifyList :: [Type] -> TI Subst
unifyList [] = return Env.empty
unifyList [_] = return Env.empty
unifyList (t : r : ts) = do
  s1 <- unify t r
  s2 <- unifyList ts
  return $ s2 <:> s1

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
generalize ctx t = Set.toList (free t \\ free ctx) :. t

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
      ("Cons", ["a"] :. Var "a" :-> tList (Var "a") :-> tList (Var "a")),
      ("Null", ["a"] :. tList (Var "a"))
    ]

-- pos = initialPos "main.pure"
-- testTI $
--   XLam
--     ( App
--         (App (Literal $ Id "Cons" pos) (Literal $ Id "x" pos) pos)
--         (Literal $ Id "xs" pos)
--         pos
--     )
--     ( App
--         (App (Literal $ Id "(+)" pos) (Literal $ Id "x" pos) pos)
--         (Literal $ Int 1 pos)
--         pos
--     )
--     pos