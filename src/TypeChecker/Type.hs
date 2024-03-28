module TypeChecker.Type where

import TypeChecker.Id (Id)
import TypeChecker.Kind (Kind (..))
import TypeChecker.PPrint
  ( Doc,
    PPrint (parPprint, pprint),
    ppParen,
    text,
    (<+>),
  )

data Type
  = -- | type variables
    TVar Tyvar
  | -- | type constructors
    TCon Tycon
  | -- | type application
    TAp Type Type
  | -- | generic
    TGen Int
  deriving (Eq)

data Tyvar = Tyvar Id Kind deriving (Eq)

data Tycon = Tycon Id Kind deriving (Eq)

instance Show Type where
  showsPrec _ x = shows $ pprint x

instance PPrint Type where
  pprint = pptype 0
  parPprint = pptype 10

pptype :: Int -> Type -> Doc
pptype d (TAp (TAp a x) y)
  | a == tArrow =
      ppParen
        (d >= 5)
        ( pptype 5 x
            <+> text "->"
            <+> pptype 0 y
        )
pptype d (TAp l r) =
  ppParen
    (d >= 10)
    ( text "TAp"
        <+> pptype 10 l
        <+> pptype 10 r
    )
pptype d (TGen n) = ppParen (d >= 10) (text $ "a" ++ show n)
pptype _ (TCon (Tycon i _)) = text ('t' : i)
pptype _ (TVar v) = pprint v

instance PPrint Tyvar where
  pprint (Tyvar v _) = text v

tUnit, tChar, tInt, tInteger, tFloat, tDouble :: Type
tUnit = TCon (Tycon "()" Star)
tChar = TCon (Tycon "Char" Star)
tInt = TCon (Tycon "Int" Star)
tInteger = TCon (Tycon "Integer" Star)
tFloat = TCon (Tycon "Float" Star)
tDouble = TCon (Tycon "Double" Star)

tList, tArrow, tTuple2 :: Type
tList = TCon (Tycon "[]" (Kfun Star Star))
tArrow = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
tTuple2 = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

tTuple3, tTuple4, tTuple5, tTuple6, tTuple7 :: Type
tTuple3 =
  TCon (Tycon "(,,)" (Kfun Star (Kfun Star (Kfun Star Star))))
tTuple4 =
  TCon (Tycon "(,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))
tTuple5 =
  TCon (Tycon "(,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))
tTuple6 =
  TCon (Tycon "(,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star)))))))
tTuple7 =
  TCon (Tycon "(,,,,,,)" (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star (Kfun Star Star))))))))

tString :: Type
tString = list tChar

infixr 4 `fn`

fn :: Type -> Type -> Type
a `fn` b = TAp (TAp tArrow a) b

list :: Type -> Type
list = TAp tList

pair :: Type -> Type -> Type
pair a = TAp (TAp tTuple2 a)

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u) = kind u
  kind (TAp t _) = case kind t of
    (Kfun _ k) -> k
    x -> error $ unwords ["expected a type function, instead got", show x]
  kind (TGen _) = error $ unwords ["TGen is not meant to be used here"]
