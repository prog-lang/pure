module Node.Prep (prepare) where

import Data.Foldable (toList)
import qualified Data.Set as Set
import Node.Error (Error (..))
import Node.Sacred (keywords)
import Pure.Typing.Module (Module (..), names)
import Utility.Result (Result (..))

prepare :: Module -> Result Error Module
prepare = namesContainKeywords

namesContainKeywords :: Module -> Result Error Module
namesContainKeywords modul =
  if null intersection
    then Ok modul
    else Err $ KeywordNames intersection
  where
    intersection = toList $ Set.intersection keywords $ names modul