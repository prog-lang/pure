{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Node.Transpiler where

import Data.Foldable (toList)
import Data.List (singleton)
import Node.Error (Error)
import qualified Node.Node as Node
import Node.Prep (prepare)
import qualified Pure.Expr as Pure
import Pure.Typing.Module (Def (..), Module (..), defs)
import Utility.Convert (Into (..))
import Utility.Result (Result)

transpile :: Module -> Result Error Node.Module
transpile = fmap into . prepare

instance Module `Into` Node.Module where
  into modul =
    Node.Module $
      concatMap into (defs modul)
        ++ [Node.Exports $ toList $ exports modul]

instance Def `Into` [Node.Statement] where
  into (Def name (Pure.Lam param expr _) _) =
    singleton $ Node.Function name [param] [Node.Return $ into expr]
  into (Def name expr _) = singleton $ Node.Const name $ into expr

-- into (Pure.TypeDef ty _ opts) = map typeConsStatement opts
--   where
--     typeConsStatement opt@(Cons tag _) = Node.Const tag $ typeCons opt
--     typeCons (Cons tag args) = foldr lam object params
--       where
--         params = numbered args
--         lam param expr = Node.Lam [param] [Node.Return expr]
--         object =
--           Node.Object
--             [ ("type", Node.Str ty),
--               ("tag", Node.Str tag),
--               ("args", Node.Array $ map Node.Id params)
--             ]

instance Pure.Expr `Into` Node.Expr where
  into (Pure.Lam param body _) = Node.Lam [param] [Node.Return $ into body]
  into (Pure.If b l r _) = Node.Ternary (into b) (into l) (into r)
  into (Pure.App e1 e2 _) = Node.Call (into e1) [into e2]
  into (Pure.List list _) = Node.Array $ map into list
  into (Pure.Id ident _) = Node.Id ident
  into (Pure.Str str _) = Node.Str str
  into (Pure.Float number _) = Node.Float number
  into (Pure.Int int _) = Node.Int int
  into (Pure.Bool bool _) = Node.Bool bool
