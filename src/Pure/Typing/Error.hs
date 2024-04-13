module Pure.Typing.Error (Error (..), printError) where

import Pure.Typing.Type (Scheme, Type)
import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)
import Utility.Common (Id)
import Utility.Pretty (printSection)
import Utility.Strings (li, ticked, ul, (+-+), (+\\+))

data Error
  = PreparationError String
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
title (PreparationError _) = "preparation error"
title (UnboundVariableError _) = "unbound variable"
title OccursCheckError = "occurs check failed"
title (UnificationError _ _) = "unification error"
title (AssertionError _ _) = "type hint mismatch"
title (WithId _ err) = title err
title (At _ err) = title err
title (Stack err _) = title err

instance Show Error where
  show (PreparationError msg) = msg
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