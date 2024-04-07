{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Infer (assert, infer, runTI, evalTI) where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, evalState, get, put, runState)
import Data.Functor ((<&>))
import Data.Map.Strict as Map (elems, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Pure.Expr (Expr (..))
import Pure.Typing.Env (Apply (..), Env (..), (<:>))
import qualified Pure.Typing.Env as Env
import Pure.Typing.Type (Scheme (..), Type (..), tBool, tFloat, tInt, tList, tStr)
import Utility.Common (Id)
import Utility.Fun ((|>))
import Utility.Pretty (printSection)
import Utility.Result (Result (..))
import qualified Utility.Result as Result
import Utility.Strings (Parens (..), (+-+), (+\+), (+\\+))

-- TYPES -----------------------------------------------------------------------

type Error = String

type Subst = Env Type

type Context = Env Scheme

type TI a = ExceptT String (State Int) a

-- FREE ------------------------------------------------------------------------

class Free a where
  -- | @free@ gets free type variables.
  free :: a -> Set Id

instance Free Type where
  free (Var v) = Set.singleton v
  free (t :-> r) = Set.union (free t) (free r)
  free (Cons _ ts) = Set.unions $ map free ts

instance Free Scheme where
  free (vars :. t) = Set.difference (free t) (Set.fromList vars)

instance Free Context where
  free (Env ctx) = foldMap free $ Map.elems ctx

-- RUN -------------------------------------------------------------------------

runTI :: TI a -> (Result Error a, Int)
runTI ti = let (s, a) = runState (runExceptT ti) 0 in (Result.fromEither s, a)

evalTI :: TI a -> Result Error a
evalTI ti = evalState (runExceptT ti) 0 |> Result.fromEither

-- ERROR REPORTING -------------------------------------------------------------

cannotUnify :: String
cannotUnify = "Cannot unify:"

throwUnboundVariableError :: Id -> TI a
throwUnboundVariableError x = throwError $ "Unbound variable:" +-+ show x

throwOccursCheckError :: TI a
throwOccursCheckError = throwError $ cannotUnify +-+ "occurs check failed"

throwUnificationError :: Type -> Type -> TI a
throwUnificationError t r =
  throwError $ cannotUnify +-+ parens t +-+ "with" +-+ parens r

throwAssertError :: Scheme -> Scheme -> TI a
throwAssertError hint type_ =
  throwError $ "Expected ::" +-+ show hint +\+ "Received ::" +-+ show type_

-- INFER & ASSERT --------------------------------------------------------------

assert :: Context -> Scheme -> Expr -> TI Scheme
assert ctx hint@(vs :. _) expr = do
  (s1, tyExpr) <- infer ctx expr
  tyHint <- instantiate hint
  s2 <- unify tyHint tyExpr
  let t@(vs' :. _) = generalize ctx (s2 <:> s1 +-> tyHint)
  if length vs == length vs'
    then return t
    else throwAssertError hint t

infer :: Context -> Expr -> TI (Subst, Type)
infer _ (Bool _) = return (Env.empty, tBool)
infer _ (Int _) = return (Env.empty, tInt)
infer _ (Float _) = return (Env.empty, tFloat)
infer _ (Str _) = return (Env.empty, tStr)
infer ctx (Id i) =
  case Env.typeOf i ctx of
    Nothing -> throwUnboundVariableError i
    Just scheme -> instantiate scheme <&> (,) Env.empty
infer _ (List []) = var <&> (,) Env.empty
infer ctx (List (x : xs)) = do
  (s1, tx) <- infer ctx x
  (s2, txs) <- infer (s1 +-> ctx) (List xs)
  s3 <- unify (tList tx) txs
  return (s3 <:> s2 <:> s1, s3 +-> txs)
infer ctx (App fun arg) = do
  (s1, tyFun) <- infer ctx fun
  (s2, tyArg) <- infer (s1 +-> ctx) arg
  tyRes <- var
  s3 <- unify (tyArg :-> tyRes) (s2 +-> tyFun)
  return (s3 <:> s2 <:> s1, s3 +-> tyRes)
infer ctx (If condition e1 e2) = do
  (s1, t1) <- infer ctx condition
  (s2, t2) <- infer (s1 +-> ctx) e1
  (s3, t3) <- infer (s2 <:> s1 +-> ctx) e2
  s4 <- unify (s3 <:> s2 <:> s1 +-> t1) tBool
  s5 <- unify (s4 <:> s3 <:> s2 <:> s1 +-> t2) t3
  let s = s5 <:> s4 <:> s3 <:> s2 <:> s1
  let t = s +-> t2
  return (s, t)
infer ctx (Lam binder body) = do
  tyBinder <- var
  let tmpCtx = Env.insert binder ([] :. tyBinder) ctx
  (s1, tyBody) <- infer tmpCtx body
  return (s1, (s1 +-> tyBinder) :-> tyBody)

-- HELPERS ---------------------------------------------------------------------

unify :: Type -> Type -> TI Subst
unify (Var u) t = varBind u t
unify t (Var u) = varBind u t
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
  s <- get
  put $ s + 1
  return $ Var $ "v" ++ show s

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

-- REPL TESTING ----------------------------------------------------------------

testAssert :: Scheme -> Expr -> IO ()
testAssert hint expr = do
  case evalTI (assert primitives hint expr) of
    Err err -> printSection "TYPE HINT MISMATCH" $ ":=" +-+ show expr +\\+ err
    Ok ok -> putStrLn $ "\nOK:" +-+ show ok ++ "\n"

testTI :: Expr -> IO ()
testTI expr = do
  case evalTI (typeInference primitives expr) of
    Err err -> printSection "INFERENCE FAILURE" $ ":=" +-+ show expr +\\+ err
    Ok t -> putStrLn $ "\n" ++ show (generalize Env.empty t) ++ "\n"

typeInference :: Context -> Expr -> TI Type
typeInference ctx expr = infer ctx expr <&> uncurry (+->)

primitives :: Context
primitives =
  Map.fromList
    [ ("id", ["a"] :. Var "a" :-> Var "a"),
      ("always", ["a", "b"] :. Var "a" :-> Var "b" :-> Var "a"),
      ("(+)", [] :. tInt :-> tInt :-> tInt),
      ("(:)", ["a"] :. Var "a" :-> tList (Var "a") :-> tList (Var "a")),
      ("null", ["a"] :. tList (Var "a"))
    ]
    |> Env
