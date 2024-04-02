module Pure.Checks
  ( afterParse,
    entrypointPresent,
  )
where

import Data.Foldable (toList)
import Data.List (nub, (\\))
import qualified Data.Set as Set
import Pure.AST
  ( Module (..),
    assignmentNames,
    moduleNames,
    typeDefNames,
    typeHintNames,
  )
import Result (Result (..))
import Strings (commad)

-- TYPES -----------------------------------------------------------------------

type Error = String

-- COMBINED --------------------------------------------------------------------

afterParse :: Module -> Result Error Module
afterParse modul =
  duplicateDefinitions modul
    >>= duplicateTypeHints
    >>= typeHintsForEachDefinition
    >>= definitionsForEachTypeHint

-- CHECKS ----------------------------------------------------------------------

duplicateDefinitions :: Module -> Result Error Module
duplicateDefinitions modul =
  if null duplicates
    then Ok modul
    else Err err
  where
    err = prefix ++ commad duplicates
    prefix = "module contains multiple definitions with the same name: "
    duplicates = ns \\ unique
    unique = nub ns
    ns = typeDefNames modul ++ assignmentNames modul

duplicateTypeHints :: Module -> Result Error Module
duplicateTypeHints modul =
  if null duplicates
    then Ok modul
    else Err err
  where
    err = prefix ++ commad duplicates
    prefix = "module contains multiple type hints of the same name: "
    duplicates = ns \\ unique
    unique = nub ns
    ns = typeHintNames modul

typeHintsForEachDefinition :: Module -> Result Error Module
typeHintsForEachDefinition modul =
  if null diff
    then Ok modul
    else Err err
  where
    err = "found definitions without type hints: " ++ elaboration
    elaboration = commad $ toList diff
    diff = Set.difference typeHints assignments
    assignments = Set.fromList $ assignmentNames modul
    typeHints = Set.fromList $ typeHintNames modul

definitionsForEachTypeHint :: Module -> Result Error Module
definitionsForEachTypeHint modul =
  if null diff
    then Ok modul
    else Err err
  where
    err = "found type hints without definitions: " ++ elaboration
    elaboration = commad $ toList diff
    diff = Set.difference assignments typeHints
    assignments = Set.fromList $ assignmentNames modul
    typeHints = Set.fromList $ typeHintNames modul

entrypointPresent :: Module -> Result Error Module
entrypointPresent modul =
  if elem entrypoint $ moduleNames modul
    then Ok modul
    else Err err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"
