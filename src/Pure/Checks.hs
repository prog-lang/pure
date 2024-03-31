module Pure.Checks (duplicateDefinitions, entrypointPresent) where

import Data.List (intercalate, nub, (\\))
import Pure.AST (Module (..), moduleNames)
import Result (Result (..))

-- TYPES

type Error = String

-- CHECKS

duplicateDefinitions :: Module -> Result Error Module
duplicateDefinitions ast =
  if null duplicates
    then Ok ast
    else Err err
  where
    err = prefix ++ intercalate ", " duplicates
    prefix = "module contains duplicate definitions: "
    duplicates = ns \\ unique
    unique = nub ns
    ns = moduleNames ast

entrypointPresent :: Module -> Result Error Module
entrypointPresent ast =
  if elem entrypoint $ moduleNames ast
    then Ok ast
    else Err err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"
