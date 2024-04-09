module Pure.Checks (entrypointPresent) where

import Pure.Parser (Module (..), moduleNames)
import Utility.Result (Result (..))

-- ERROR -----------------------------------------------------------------------

type Error = String

-- CHECKS ----------------------------------------------------------------------

entrypointPresent :: Module -> Result Error Module
entrypointPresent modul =
  if elem entrypoint $ moduleNames modul
    then Ok modul
    else Err err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"
