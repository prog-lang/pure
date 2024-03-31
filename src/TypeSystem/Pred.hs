module TypeSystem.Constraint where

import Control.Monad (msum)
import Data.HashMap.Strict (HashMap, insert)
import qualified Data.HashMap.Strict as HashMap
import Data.List (union)
import Data.Maybe (isJust)
import TypeSystem.Base
  ( Id,
    Type (..),
    TypeVar (..),
    floatT,
    intT,
  )
import TypeSystem.Subst (Typed (..))
import TypeSystem.Unify (Match (..), Unify (..))
import Prelude hiding (lookup)

--- QUALIFIED ------------------------------------------------------------------

infixr 1 :=>

-- | Constructor for a type qualified by a list of constraints.
--
-- Given a function like @toInt :: (Num a) => a -> Int@,
-- it's type can be represented as:
--
-- @
-- toIntT :: Qualified Type
-- toIntT = [IsIn "Num" a] :=> a --> TCon (TypeCon "Int" Star)
--   where a = TVar $ TypeVar "a" Star
-- @
data Qualified t = [Constraint] :=> t deriving (Eq)

--- CONSTRAINT -----------------------------------------------------------------

-- | Constraint @Show a@ can be represented as
--  @IsIn "Show" [TVar $ TypeVar "a" Star]@.
data Constraint = IsIn Id Type deriving (Eq)

constrained :: Constraint -> Id
constrained (IsIn i _) = i

instance (Typed t) => Typed (Qualified t) where
  apply s (ps :=> t) = apply s ps :=> apply s t
  typeVar (ps :=> t) = typeVar ps `union` typeVar t

instance Typed Constraint where
  apply s (IsIn i ts) = IsIn i (apply s ts)
  typeVar (IsIn _ ts) = typeVar ts

instance Unify Constraint where
  mgu = lift mgu

instance Match Constraint where
  match = lift match

lift ::
  (MonadFail m) =>
  (Type -> Type -> m a) ->
  Constraint ->
  Constraint ->
  m a
lift m (IsIn name ts) (IsIn name' ts')
  | name == name' = m ts ts'
  | otherwise = fail "classes differ"

--- TRAIT ----------------------------------------------------------------------

-- | A Trait is defined as a list of constrained type variables.
type Trait = ([TypeVar], [Constraint], [Instance])

type Instance = Qualified Constraint

--- TRAIT ENVIRONMENT ----------------------------------------------------------

data TraitEnv = TraitEnv
  { env :: HashMap Id Trait,
    defaults :: [Type]
  }

lookup :: Id -> TraitEnv -> Maybe Trait
lookup name = HashMap.lookup name . env

sig :: TraitEnv -> Id -> [TypeVar]
sig tenv name = case lookup name tenv of Just (vs, _, _) -> vs

super :: TraitEnv -> Id -> [Constraint]
super tenv name = case lookup name tenv of Just (_, is, _) -> is

insts :: TraitEnv -> Id -> [Instance]
insts tenv name = case lookup name tenv of Just (_, _, its) -> its

defined :: Maybe a -> Bool
defined = isJust

modify :: TraitEnv -> Id -> Trait -> TraitEnv
modify ce i c = ce {env = insert i c $ env ce}

initialEnv :: TraitEnv
initialEnv =
  TraitEnv
    { env = HashMap.empty,
      defaults = [intT, floatT]
    }

type EnvTransformer = TraitEnv -> Maybe TraitEnv

infixr 5 <:>

(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = f ce >>= g

addTrait :: Id -> [TypeVar] -> [Constraint] -> EnvTransformer
addTrait name tyvars preds tenv
  | defined (lookup name tenv) = fail "class already defined"
  | not (all (defined . flip lookup tenv . constrained) preds) = fail "superclass not defined"
  | otherwise = return $ modify tenv name (tyvars, preds, [])

-- addPreludeTraites :: EnvTransformer
-- addPreludeTraites = addCoreTraites <:> addNumTraites

-- atyvar :: TypeVar
-- atyvar = TypeVar "a" Star

-- atype :: Type
-- atype = TVar atyvar

-- asig :: [TypeVar]
-- asig = [atyvar]

-- mtyvar :: TypeVar
-- mtyvar = TypeVar "m" (Star :-> Star)

-- mtype :: Type
-- mtype = TVar mtyvar

-- msig :: [TypeVar]
-- msig = [mtyvar]

-- addCoreTraites :: EnvTransformer
-- addCoreTraites =
--   addTrait "Eq" asig []
--     <:> addTrait "Ord" asig [IsIn "Eq" [atype]]
--     <:> addTrait "Show" asig []
--     <:> addTrait "Read" asig []
--     <:> addTrait "Bounded" asig []
--     <:> addTrait "Enum" asig []
--     <:> addTrait "Functor" msig []
--     <:> addTrait "Monad" msig []

-- addNumTraites :: EnvTransformer
-- addNumTraites =
--   addTrait
--     "Num"
--     asig
--     [ IsIn "Eq" [atype],
--       IsIn "Show" [atype]
--     ]
--     <:> addTrait
--       "Real"
--       asig
--       [ IsIn "Num" [atype],
--         IsIn "Ord" [atype]
--       ]
--     <:> addTrait "Fractional" asig [IsIn "Num" [atype]]
--     <:> addTrait
--       "Integral"
--       asig
--       [ IsIn "Real" [atype],
--         IsIn "Enum" [atype]
--       ]
--     <:> addTrait
--       "RealFrac"
--       asig
--       [ IsIn "Real" [atype],
--         IsIn "Fractional" [atype]
--       ]
--     <:> addTrait "Floating" asig [IsIn "Fractional" [atype]]
--     <:> addTrait
--       "RealFloat"
--       asig
--       [ IsIn "RealFrac" [atype],
--         IsIn "Floating" [atype]
--       ]

-- addInst :: [Constraint] -> Constraint -> EnvTransformer
-- addInst ps p@(IsIn i _) ce
--   | not (defined (lookup ce i)) = fail "no class for instance"
--   | any (overlap p) qs = fail "overlapping instance"
--   | otherwise = return (modify ce i c)
--   where
--     its = insts ce i
--     qs = [q | (_ :=> q) <- its]
--     c = (sig ce i, super ce i, (ps :=> p) : its)

-- overlap :: Constraint -> Constraint -> Bool
-- overlap p q = defined (mgu p q)

-- exampleInsts :: EnvTransformer
-- exampleInsts =
--   addPreludeTraites
--     <:> addInst [] (IsIn "Ord" [tUnit])
--     <:> addInst [] (IsIn "Ord" [tChar])
--     <:> addInst [] (IsIn "Ord" [tInt])
--     <:> addInst
--       [ IsIn "Ord" [TVar (TypeVar "a" Star)],
--         IsIn "Ord" [TVar (TypeVar "b" Star)]
--       ]
--       ( IsIn
--           "Ord"
--           [ pair
--               (TVar (TypeVar "a" Star))
--               (TVar (TypeVar "b" Star))
--           ]
--       )

-- -----------------------------------------------------------------------------

-- bySuper :: TraitEnv -> Constraint -> [Constraint]
-- bySuper ce p@(IsIn i ts) =
--   p : concat (map (bySuper ce) supers)
--   where
--     supers = apply s (super ce i)
--     s = zip (sig ce i) ts

-- byInst :: TraitEnv -> Constraint -> Maybe [Constraint]
-- byInst ce p@(IsIn i t) = msum [tryInst it | it <- insts ce i]
--   where
--     tryInst (ps :=> h) = do
--       u <- match h p
--       Just (map (apply u) ps)

-- entail :: TraitEnv -> [Constraint] -> Constraint -> Bool
-- entail ce ps p =
--   any (p `elem`) (map (bySuper ce) ps)
--     || case byInst ce p of
--       Nothing -> False
--       Just qs -> all (entail ce ps) qs

-- -----------------------------------------------------------------------------

-- simplify :: ([Constraint] -> Constraint -> Bool) -> [Constraint] -> [Constraint]
-- simplify ent = loop []
--   where
--     loop rs [] = rs
--     loop rs (p : ps)
--       | ent (rs ++ ps) p = loop rs ps
--       | otherwise = loop (p : rs) ps

-- reduce :: TraitEnv -> [Constraint] -> [Constraint]
-- reduce ce = simplify (scEntail ce) . elimTauts ce

-- elimTauts :: TraitEnv -> [Constraint] -> [Constraint]
-- elimTauts ce ps = [p | p <- ps, not (entail ce [] p)]

-- scEntail :: TraitEnv -> [Constraint] -> Constraint -> Bool
-- scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
