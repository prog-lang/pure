{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

module Node.Node
  ( Module (..),
    Statement (..),
    Expr (..),
  )
where

import qualified Node.Sacred as S
import Strings (array, braced, commad, list, parenthesised, tuple, (+-+))

-- TYPES

type Id = String

newtype Module = Module [Statement]

data Statement
  = Const Id Expr
  | Var Id Expr
  | Let Id Expr
  | Assign Id Expr
  | Return Expr
  | Exports [Id]
  | Function Id [Id] [Statement]
  deriving (Eq)

data Expr
  = Bool Bool
  | Int Integer
  | Float Double
  | Str String
  | Id Id
  | Array [Expr]
  | Object [(String, Expr)]
  | New Id [Expr]
  | Ternary Expr Expr Expr
  | Call Expr [Expr]
  | Lam [Id] [Statement]
  deriving (Eq)

-- INSPECT

isTernary :: Expr -> Bool
isTernary (Ternary {}) = True
isTernary _ = False

-- SHOW

embrace :: Expr -> String
embrace i@(Int _) = show i
embrace f@(Float _) = show f
embrace s@(Str _) = show s
embrace i@(Id _) = show i
embrace arr@(Array _) = show arr
embrace ex = parenthesised $ show ex

embraceIf :: (Expr -> Bool) -> Expr -> String
embraceIf check ex = if check ex then embrace ex else show ex

instance Show Module where
  show (Module ss) = unlines $ map show ss

instance Show Statement where
  show (Const ident ex) = S.const +-+ ident +-+ S.assign +-+ show ex ++ S.semicolon
  show (Var ident ex) = S.var +-+ ident +-+ S.assign +-+ show ex ++ S.semicolon
  show (Let ident ex) = S.let_ +-+ ident +-+ S.assign +-+ show ex ++ S.semicolon
  show (Assign ident ex) = ident +-+ S.assign +-+ show ex ++ S.semicolon
  show (Return ex) = S.return +-+ show ex ++ S.semicolon
  show (Exports ids) = S.exports +-+ S.assign +-+ array ids
  show (Function name ids ss) = S.function +-+ name ++ params +-+ body
    where
      params = tuple ids
      body = braced $ unwords $ map show ss

instance Show Expr where
  show (Bool bool) = if bool then S.true else S.false
  show (Int i) = show i
  show (Float n) = show n
  show (Str s) = show s
  show (Id ident) = ident
  show (Array exs) = list $ map show exs
  show (Object kvs) = braced $ commad $ map (\(k, v) -> k ++ S.colon +-+ show v) kvs
  show (New ident exs) = S.new ++ ident ++ tuple (map show exs)
  show (Ternary q l r) = cond_ +-+ S.question +-+ then_ +-+ S.colon +-+ else_
    where
      cond_ = show q
      then_ = embraceIf isTernary l
      else_ = embraceIf isTernary r
  show (Call ex exs) = embrace ex ++ tuple (map show exs)
  show (Lam ids ss) = S.function ++ params +-+ body
    where
      params = tuple ids
      body = braced $ unwords $ map show ss