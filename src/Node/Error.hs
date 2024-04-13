module Node.Error (Error (..), printError) where

import Utility.Common (Id)
import Utility.Pretty (printSection)
import Utility.Strings (commad, li, ul)

data Error = KeywordNames [Id]

printError :: Error -> IO ()
printError err = printSection (title err) (show err)

title :: Error -> String
title (KeywordNames _) = "reserved keywords"

instance Show Error where
  show (KeywordNames names) =
    ul
      [ "Some names in this module happen to be reserved JavaScript keywords.",
        "You can't use these as identifiers in your program:\n",
        li $ commad names
      ]
