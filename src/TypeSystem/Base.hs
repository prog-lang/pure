module TypeSystem.Base
  ( Id,
    makeId,
    makeGen,
    Kind (..),
    HasKind (..),
    makeKind,
    Type (..),
    TypeVar (..),
    TypeCon (..),
  )
where

import Strings ((+-+))

--- ID -------------------------------------------------------------------------

type Id = String

makeId :: Int -> Id
makeId = ("$" ++) . show

makeGen :: Int -> Id
makeGen = ("_" ++) . show

--- KIND -----------------------------------------------------------------------

data Kind = Star | Kind :-> Kind deriving (Eq)

instance Show Kind where
  show Star = "*"
  show (a :-> b) = show a +-+ "->" +-+ show b

makeKind :: Int -> Kind
makeKind i
  | i <= 0 = Star
  | otherwise = Star :-> makeKind (i - 1)

class HasKind t where
  kind :: t -> Kind

instance HasKind TypeVar where
  kind (TypeVar _ k) = k

instance HasKind TypeCon where
  kind (TypeCon _ k) = k

instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u) = kind u
  kind (TApp t _) = case kind t of
    (_ :-> k) -> k
    x -> error $ "expected a type function, instead got" +-+ show x
  kind (TGen _) = error "TGen doesn't have a Kind"

--- TYPE -----------------------------------------------------------------------

data Type
  = -- | type variables
    TVar TypeVar
  | -- | type constructors
    TCon TypeCon
  | -- | type application
    TApp Type Type
  | -- | generic
    TGen Int
  deriving (Eq)

data TypeVar = TypeVar Id Kind deriving (Eq)

data TypeCon = TypeCon Id Kind deriving (Eq)

instance Show Type where
  show (TVar a) = show a
  show (TCon a) = show a
  show (TApp a b) = show a +-+ show b
  show (TGen a) = makeGen a

instance Show TypeVar where
  show (TypeVar name _) = name

instance Show TypeCon where
  show (TypeCon name _) = name
