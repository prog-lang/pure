{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Node.Transpiler where

import Convert (Into (..))
import Data.List (singleton)
import qualified Node.Node as Node
import Pure.Parser (Definition ((:=)))
import qualified Pure.Parser as Pure
import Strings (numbered)

type Error = String

instance Into Node.Module String where
  into = show

instance Into Pure.Module Node.Module where
  into modul =
    Node.Module $
      concatMap into (Pure.definitions modul)
        ++ [Node.Exports $ Pure.exports modul]

-- Pure.TypeDefs map to multiple Node.Statements
instance Into Pure.Definition [Node.Statement] where
  into (name := (Pure.Lam param expr)) = singleton $ Node.Function name [param] [Node.Return $ into expr]
  into (name := expr) = singleton $ Node.Const name $ into expr
  into (Pure.TypeDef ty _ opts) = map typeConsStatement opts
    where
      typeConsStatement opt@(Pure.Type tag _) = Node.Const tag $ typeCons opt
      typeCons (Pure.Type tag args) = foldr lam object params
        where
          params = numbered args
          lam param expr = Node.Lam [param] [Node.Return expr]
          object =
            Node.Object
              [ ("type", Node.Str ty),
                ("tag", Node.Str tag),
                ("args", Node.Array $ map Node.Id params)
              ]

instance Into Pure.Expr Node.Expr where
  into (Pure.Lam param body) = Node.Lam [param] [Node.Return $ into body]
  into (Pure.If b l r) = Node.Ternary (into b) (into l) (into r)
  into (Pure.App ex exs) = foldl call (into ex) exs
    where
      call f arg = Node.Call f [into arg]
  into (Pure.List list) = Node.Array $ map into list
  into (Pure.Id ident) = Node.Id ident
  into (Pure.Str str) = Node.Str str
  into (Pure.Float number) = Node.Float number
  into (Pure.Int int) = Node.Int int
  into (Pure.Bool bool) = Node.Bool bool
