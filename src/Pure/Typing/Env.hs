module Pure.Typing.Env
  ( TypeEnv,
    member,
    typeOf,
    assert,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Pure.Typing.Type (Type)
import Utility.Common (Id)
import Utility.Result (Result (..))

type TypeEnv = Map Id Type

member :: Id -> TypeEnv -> Bool
member = Map.member

typeOf :: Id -> TypeEnv -> Maybe Type
typeOf = Map.lookup

assert :: Type -> Id -> TypeEnv -> Result (Maybe Type) Type
assert t i env =
  case typeOf i env of
    Nothing -> Err Nothing
    Just t' -> if t == t' then Ok t' else Err $ Just t'