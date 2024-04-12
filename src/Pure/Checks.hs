module Pure.Checks (entrypointPresent) where

import Pure.Typing.Module (Module (..))
import qualified Pure.Typing.Module as Module
import Utility.Result (Result (..))

-- ERROR -----------------------------------------------------------------------

type Error = String

-- CHECKS ----------------------------------------------------------------------

entrypointPresent :: Module -> Result Error Module
entrypointPresent modul =
  if elem entrypoint $ Module.names modul
    then Ok modul
    else Err err
  where
    err = "entrypoint missing: " ++ entrypoint
    entrypoint = "main"
