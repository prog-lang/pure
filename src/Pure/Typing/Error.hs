module Pure.Typing.Error (Error (..), printError) where

import Pure.Typing.Type (Scheme, Type)
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)
import Utility.Common (Id)
import Utility.Pretty (printSection)
import Utility.Strings (commad, li, ticked, ul, (+-+), (+\\+))

data Error
  = ModuleExportsUndefinedIdentifier Id
  | TypeHintMissesDefinition Id
  | DefinitionMissesTypeHint Id
  | UnboundTypeVariablesInTypeDef Id [Id]
  | UnusedTypeVariablesInTypeDef Id [Id]
  | UnboundVariableError Id
  | OccursCheckError
  | UnificationError Type Type
  | AssertionError Scheme Scheme
  | WithId Id Error
  | At SourcePos Error
  | Stack Error Error

printError :: Error -> IO ()
printError err = printSection (title err) (show err)

title :: Error -> String
title (ModuleExportsUndefinedIdentifier _) = "module exports undefined name"
title (TypeHintMissesDefinition _) = "definition wanted"
title (DefinitionMissesTypeHint _) = "type hint wanted"
title (UnboundTypeVariablesInTypeDef _ _) = "unbound type variables"
title (UnusedTypeVariablesInTypeDef _ _) = "unused type variables"
title (UnboundVariableError _) = "unbound variable"
title OccursCheckError = "occurs check failed"
title (UnificationError _ _) = "unification error"
title (AssertionError _ _) = "type hint mismatch"
title (WithId _ err) = title err
title (At _ err) = title err
title (Stack err _) = title err

instance Show Error where
  show (ModuleExportsUndefinedIdentifier name) =
    ul
      [ "I don't think I saw" +-+ ticked name +-+ "anywhere\n",
        "Are you sure you have defined it?",
        "You might have renamed it or made a typo.",
        "Go through your code carefully one more time!"
      ]
  show (TypeHintMissesDefinition name) =
    ul
      [ "I found a type hint for" +-+ ticked name,
        "but I can't find its definition anywhere\n",
        "You must provide a definition for each type hint!",
        "Otherwise, I won't know what to do with it."
      ]
  show (DefinitionMissesTypeHint name) =
    ul
      [ "I found the definition of" +-+ ticked name,
        "but I got stuck trying to find its type hint\n",
        "You must provide a type hint for each top-level definition!",
        "Otherwise, type checking would be too hard for me to do. I need your help."
      ]
  show (UnboundTypeVariablesInTypeDef name tvs) =
    ul
      [ "I found unbound type variables in" +-+ ticked name ++ "\n",
        li $ commad tvs ++ "\n",
        "You are only allowed to use polymorphic type variables listed next to",
        "the type's name.\n",
        "Consider the following example:\n",
        li "type Maybe a is ...",
        li "           ^ This is the only type variable",
        li $ "             you can use within" +-+ ticked "Maybe"
      ]
  show (UnusedTypeVariablesInTypeDef name tvs) =
    ul
      [ "I found unused type variables in" +-+ ticked name ++ "\n",
        li $ commad tvs ++ "\n",
        "You must use every polymorphic type variable listed next to the",
        "type's name.\n",
        "Consider the following example:\n",
        li "type Result err ok is ...",
        li "            ^^^ ^^ You must utilise both of these",
        li $ "                   type variables within" +-+ ticked "Result"
      ]
  show (UnboundVariableError v) = "I can't resolve this name:" +-+ ticked v
  show OccursCheckError = "Occurs check blew everything up!"
  show (UnificationError t t') =
    "I can't unify"
      +\\+ li (show t)
      +\\+ "with"
      +\\+ li (show t')
  show (AssertionError s s') =
    "The type hint suggested it should look like"
      +\\+ li (show s)
      +\\+ "but it looks more like"
      +\\+ li (show s')
  show (WithId name err) =
    ul ["I got stuck checking the type of" +-+ ticked name ++ "\n", show err]
  show (At pos err) =
    "-->"
      +-+ sourceName pos
      +-+ "at"
      +-+ ( "line" +-+ show (sourceLine pos)
              ++ "," +-+ "column" +-+ show (sourceColumn pos)
          )
      +\\+ show err
  show (Stack err errs) = show err +\\+ show errs