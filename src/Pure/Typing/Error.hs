module Pure.Typing.Error (Error (..), printError) where

import qualified Pure.Typing.Infer as Infer
import Utility.Common (Id)
import Utility.Pretty (printSection)
import Utility.Strings (ticked, ul, (+-+))

data Error
  = PreparationError String
  | InferenceError Id Infer.Error

printError :: Error -> IO ()
printError err = printSection (title err) (show err)

title :: Error -> String
title (PreparationError _) = "preparation error"
title (InferenceError {}) = "inference error"

instance Show Error where
  show (PreparationError msg) = msg
  show (InferenceError name (Infer.UnboundVariableError v)) =
    ul
      [ "I got stuck checking the type of" +-+ ticked name ++ ".",
        "I can't resolve this name:" +-+ ticked v ++ "."
      ]
  show (InferenceError name Infer.OccursCheckError) =
    ul
      [ "I got stuck checking the type of" +-+ ticked name ++ ".",
        "Occurs check blew everything up!"
      ]
  show (InferenceError name (Infer.UnificationError t t')) =
    ul
      [ "I got stuck checking the type of" +-+ ticked name ++ ".",
        "I can't unify"
          +-+ ticked (show t)
          +-+ "with"
          +-+ ticked (show t')
          ++ "."
      ]
  show (InferenceError name (Infer.AssertionError s s')) =
    ul
      [ "I got stuck checking the type of" +-+ ticked name ++ ".",
        "You said that"
          +-+ ticked name
          +-+ "should be"
          +-+ ticked (show s)
          +-+ "but it looks more like"
          +-+ ticked (show s')
          ++ "."
      ]