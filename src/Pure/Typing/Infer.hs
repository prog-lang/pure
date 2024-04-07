{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pure.Typing.Infer where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (State, get, put, runState)
import Data.Functor ((<&>))
import Data.Map.Strict as Map (elems, fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Pure.Expr (Expr (..))
import Pure.Typing.Ctx (Ctx (..))
import qualified Pure.Typing.Ctx as Ctx
import Pure.Typing.Env (Apply (..), Env, (<:>))
import qualified Pure.Typing.Env as Env
import Pure.Typing.Type (Scheme (..), Type (..), tBool, tFloat, tInt, tList, tStr)
import System.Console.Terminal.Size (Window (..))
import qualified System.Console.Terminal.Size as Console
import Utility.Common (Id)
import Utility.Fun ((|>))
import Utility.Result (Result (..))
import qualified Utility.Result as Result
import Utility.Strings ((+-+), (+\+), (+\\+))

type Error = String

type TI a = ExceptT String (State Int) a

runTI :: TI a -> (Result Error a, Int)
runTI ti = let (s, a) = runState (runExceptT ti) 0 in (Result.fromEither s, a)

var :: TI Type
var = do
  s <- get
  put (s + 1)
  return $ Var $ "v" ++ show s

instance Free Type where
  free (Var v) = Set.singleton v
  free (t :-> r) = Set.union (free t) (free r)
  free _ = Set.empty

instance Free Scheme where
  free (vars :. t) = Set.difference (free t) (Set.fromList vars)

class Free a where
  -- | @free@ gets free type variables.
  free :: a -> Set Id

-- | Creates a fresh unification variable and binds it to the given type
varBind :: Id -> Type -> TI Env
varBind v ty
  | ty == Var v = return Env.empty
  | Set.member v (free ty) = throwError "occurs check failed"
  | otherwise = return $ Env.bind v ty

unify :: Type -> Type -> TI Env
unify (Var u) t = varBind u t
unify t (Var u) = varBind u t
unify t@(Cons i []) r@(Cons i' [])
  | i == i' = return Env.empty
  | otherwise = throwError $ "cons unification error:" +-+ show t +-+ "vs" +-+ show r
unify t@(Cons i (x : xs)) r@(Cons i' (y : ys))
  | i == i' = do
      s <- unify x y
      ss <- unify (Cons i (s +-> xs)) (Cons i (s +-> ys))
      return $ ss <:> s
  | otherwise = throwError $ "cons list unification error:" +-+ show t +-+ "vs" +-+ show r
unify (l :-> r) (l' :-> r') = do
  s1 <- unify l l'
  s2 <- unify (s1 +-> r) (s1 +-> r')
  return $ s2 <:> s1
unify t r = throwError $ "unification error:" +-+ show t +-+ "vs" +-+ show r

type Context = Ctx Scheme

instance Free Context where
  free (Ctx ctx) = foldMap free $ Map.elems ctx

generalize :: Context -> Type -> Scheme
generalize ctx t = Set.toList (Set.difference (free t) (free ctx)) :. t

instantiate :: Scheme -> TI Type
instantiate (vars :. ty) = do
  newVars <- traverse (const var) vars
  let subst = Map.fromList $ zip vars newVars
  return $ subst +-> ty

-- INFER -----------------------------------------------------------------------

assert :: Context -> Scheme -> Expr -> TI Scheme
assert ctx hint@(vs :. _) expr = do
  (s1, tyExpr) <- infer ctx expr
  tyHint <- instantiate hint
  s2 <- unify tyHint tyExpr
  let t@(vs' :. _) = generalize ctx (s2 <:> s1 +-> tyHint)
  if length vs == length vs'
    then return t
    else throwError $ "assertion failed:" +-+ show hint +-+ "-/->" +-+ show t

infer :: Context -> Expr -> TI (Env, Type)
infer _ (Bool _) = return (Env.empty, tBool)
infer _ (Int _) = return (Env.empty, tInt)
infer _ (Float _) = return (Env.empty, tFloat)
infer _ (Str _) = return (Env.empty, tStr)
infer ctx (Id i) =
  case Ctx.typeOf i ctx of
    Nothing -> throwError $ "unbound variable: " ++ show i
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
  let tmpCtx = Ctx.insert binder ([] :. tyBinder) ctx
  (s1, tyBody) <- infer tmpCtx body
  return (s1, (s1 +-> tyBinder) :-> tyBody)

typeInference :: Context -> Expr -> TI Type
typeInference ctx expr = infer ctx expr <&> uncurry (+->)

testAssert :: Scheme -> Expr -> IO ()
testAssert hint expr = do
  let (res, _) = runTI (assert primitives hint expr)
  case res of
    Err err -> section "ERROR" $ "::" +-+ show hint +\+ ":=" +-+ show expr +\\+ err
    Ok ok -> putStrLn $ "\n  OK:" +-+ show ok ++ "\n"

section :: String -> String -> IO ()
section title body = do
  maybeSize <- Console.size
  let defaultWidth = 80
  let w = maybe defaultWidth width maybeSize
  putStrLn $ "\n" ++ br w title +\\+ body ++ "\n"

br :: Int -> String -> String
br width_ message = if message == "" then line else left +-+ message +-+ right
  where
    line = take width_ dash
    left = take 3 dash
    right = take (width_ - length left - length message - 2) dash
    dash = repeat '-'

testTI :: Expr -> IO ()
testTI e = do
  let (res, _) = runTI (typeInference primitives e)
  case res of
    Err err -> putStrLn $ show e ++ "\n  " ++ err ++ "\n"
    Ok t -> putStrLn $ "\n" ++ show (generalize Ctx.empty t) ++ "\n"

primitives :: Context
primitives =
  Map.fromList
    [ ("id", ["a"] :. Var "a" :-> Var "a"),
      ("always", ["a", "b"] :. Var "a" :-> Var "b" :-> Var "a"),
      ("(+)", [] :. tInt :-> tInt :-> tInt),
      ("(:)", ["a"] :. Var "a" :-> tList (Var "a") :-> tList (Var "a")),
      ("null", ["a"] :. tList (Var "a"))
    ]
    |> Ctx
