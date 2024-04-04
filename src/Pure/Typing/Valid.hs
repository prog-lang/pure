module Pure.Typing.Valid (validate) where

import Pure.Expr (Expr)
import qualified Pure.PreType as Pre
import Pure.Typing.Type
  ( Def (..),
    Type (..),
    TypeDef (..),
    TypeEnv,
    Typed (..),
  )
import Utility.Common (Id)
import Utility.Result (Result (..))

-- TYPES -----------------------------------------------------------------------

type Error = String

-- VALIDATE --------------------------------------------------------------------

validate :: Pre.Module -> Result Error Pre.Module
validate = undefined

validateWithEnv :: TypeEnv -> Pre.Module -> Result Error Pre.Module
validateWithEnv env modul = undefined

-- validateTypeDef :: TypeDef ->
