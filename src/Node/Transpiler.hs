{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Node.Transpiler where

import Data.Foldable (toList)
import Node.Error (Error)
import qualified Node.Node as Node
import Node.Prep (prepare)
import qualified Pure.Expr as Pure
import Pure.Typing.Module (Def (..), Module (..), TypeCons (..), cons, defs)
import Pure.Typing.Type (Scheme (..), called, final, numberOfParams)
import Utility.Convert (Into (..))
import Utility.Fun ((|>))
import Utility.Result (Result)
import Utility.Strings (base26)

transpile :: Module -> Result Error Node.Module
transpile = fmap into . prepare

instance Module `Into` Node.Module where
  into modul =
    Node.Module $
      map into (cons modul)
        ++ map into (defs modul)
        ++ [Node.Exports $ toList $ exports modul]

instance TypeCons `Into` Node.Statement where
  into (TypeCons tag (_ :. ty)) = Node.Const tag $ foldr lam object params
    where
      lam param expr = Node.Lam [param] [Node.Return expr]
      params = take (numberOfParams ty) [0 ..] |> map base26
      object =
        Node.Object
          [ ("type", Node.Str $ called $ final ty),
            ("cons", Node.Str tag),
            ("args", Node.Array $ map Node.Id params)
          ]

instance Def `Into` Node.Statement where
  into (Def name (Pure.Lam param expr _) _) =
    Node.Function name [param] [Node.Return $ into expr]
  into (Def name expr _) = Node.Const name $ into expr

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
