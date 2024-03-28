module TypeChecker.Id (Id, enumId) where

type Id = String

enumId :: Int -> Id
enumId n = "$" ++ show n