{-# LANGUAGE FlexibleInstances #-}

-- | This module defines @Pure.Module@. A @Module@ is extracted from source code
-- by means of parsing (look at @module Pure.Parser@).
--
-- In other words @Pure.Module@ is an AST of a module parsed from source.
-- It is a result of a successful parse but that is all we know about it.
--
-- For instance: @type Maybe a b c d e f g is | Some | None;@
--
-- This @Maybe@ type declaration is syntactically correct but we can see that
-- there is a bunch of unused polymorphic type parameters. We will have to deal
-- with this later through type checking.
module Pure.AST
  ( Module (..),
    Id,
    moduleNames,
    Definition (..),
    defName,
    Type (..),
    Expr (..),
  )
where

import qualified Pure.Sacred as S
import Strings
  ( Parens (..),
    list,
    parenthesised,
    tuple,
    (+-+),
    (+\+),
  )

-- TYPES -----------------------------------------------------------------------

type Id = String

data Type
  = Func Type Type
  | Type Id [Type]

data Module = Module
  { definitions :: [Definition],
    exports :: [Id]
  }

data Definition
  = Id := Expr -- main := 42;
  | TypeDef Id [Id] [Type] -- type Maybe a is | Just a | Nothing;
  | TypeHint Id Type -- main :: List Str -> IO Unit;

data Expr
  = Lam Id Expr
  | If Expr Expr Expr
  | App Expr [Expr]
  | List [Expr]
  | Id Id
  | Str String
  | Float Double
  | Int Integer
  | Bool Bool
  deriving (Eq)

-- INSPECT ---------------------------------------------------------------------

moduleNames :: Module -> [Id]
moduleNames (Module defs _) = map defName defs

defName :: Definition -> Id
defName (name := _) = name
defName (TypeDef name _ _) = name
defName (TypeHint name _) = name

-- SHOW ------------------------------------------------------------------------

instance Show Module where
  show (Module defs es) = unlines $ export : map show defs
    where
      export = S.export +-+ tuple es ++ S.str S.semicolon

instance Show Definition where
  show (name := expr) = name +-+ S.walrus +-+ show expr ++ S.str S.semicolon
  show (TypeDef name poly cons) =
    S.type_
      +-+ name
      +-+ unwords poly
      +-+ S.is
      +\+ unlines (map ((S.str S.bar +-+) . show) cons)
      ++ S.str S.semicolon
  show (TypeHint name ty) = name +-+ S.typed +-+ show ty

instance Parens Type where
  parens this@(Type _ []) = show this
  parens t = parenthesised $ show t

instance Show Type where
  show (Func a b) = show a +-+ S.arrow +-+ show b
  show (Type tag []) = tag
  show (Type tag args) = tag +-+ unwords (map parens args)

instance Show Expr where
  show (Bool bool) = show bool
  show (Int int) = show int
  show (Float number) = show number
  show (Str str) = show str
  show (Id ident) = ident
  show (List l) = list (map show l)
  show (App ex exs) = unwords $ map parens (ex : exs)
  show (If x y z) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex) = p +-+ S.arrow +-+ show ex

-- PARENS ----------------------------------------------------------------------

instance Parens Expr where
  parens i@(Int _) = show i
  parens f@(Float _) = show f
  parens s@(Str _) = show s
  parens i@(Id _) = show i
  parens l@(List _) = show l
  parens ex = parenthesised $ show ex