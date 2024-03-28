module TypeChecker.Kind where

import TypeChecker.PPrint
  ( Doc,
    PPrint (parPprint, pprint),
    ppParen,
    text,
    (<+>),
  )

data Kind = Star | Kfun Kind Kind
  deriving (Eq)

instance Show Kind where
  showsPrec _ x = shows (pprint x)

instance PPrint Kind where
  pprint = ppkind 0
  parPprint = ppkind 10

ppkind :: Int -> Kind -> Doc
ppkind _ Star = text "Star"
ppkind d (Kfun l r) =
  ppParen
    (d >= 10)
    (text "Kfun" <+> ppkind 10 l <+> ppkind 0 r)

-----------------------------------------------------------------------------
