{-# LANGUAGE FlexibleInstances #-}

module Pure.Typing.Free (Free (..)) where

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Pure.Typing.Env (Context, Env (..))
import Pure.Typing.Type (Scheme (..), Type (..))
import Utility.Common (Id)

-- FREE ------------------------------------------------------------------------

class Free a where
  -- | @free@ gets free type variables.
  free :: a -> Set Id

instance Free Type where
  free (Var v) = Set.singleton v
  free (Rigid v) = Set.singleton v
  free (t :-> r) = Set.union (free t) (free r)
  free (Cons _ ts) = Set.unions $ map free ts

instance Free Scheme where
  free (vars :. t) = Set.difference (free t) (Set.fromList vars)

instance Free Context where
  free (Env ctx) = foldMap free $ Map.elems ctx