module TypeChecker.Scheme where

import TypeChecker.Kind (Kind)
import TypeChecker.PPrint
  ( PPrint (parPprint, pprint),
    nest,
    text,
    ($$),
    (<+>),
  )
import TypeChecker.Pred (Qual (..))
import TypeChecker.Subst (Types (..))
import TypeChecker.Type (HasKind (kind), Type (TGen), Tyvar)

data Scheme = Forall [Kind] (Qual Type)
  deriving (Eq)

instance Show Scheme where
  showsPrec _ x = shows (pprint x)

instance PPrint Scheme where
  pprint (Forall ks qt) =
    (text "Forall" <+> pprint ks) $$ nest 2 (parPprint qt)

instance Types Scheme where
  apply s (Forall ks qt) = Forall ks (apply s qt)
  tv (Forall _ qt) = tv qt

quantify :: [Tyvar] -> Qual Type -> Scheme
quantify vs qt = Forall ks (apply s qt)
  where
    vs' = [v | v <- tv qt, v `elem` vs]
    ks = map kind vs'
    s = zip vs' (map TGen [0 ..])

toScheme :: Type -> Scheme
toScheme t = Forall [] ([] :=> t)
