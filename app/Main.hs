module Main (main) where

import Data.Either.Extra (fromEither)
import Fun ((!>))
import Pure.AST (Module)
import Pure.Source.Checks (duplicateDefinitions)
import Pure.Source.Parser (parseModule)

main :: IO ()
main = interact readParseAndShow

readParseAndShow :: String -> String
readParseAndShow input =
  case parseModule "main.pure" input of
    Left err -> show err
    Right parsed -> check parsed

check :: Module -> String
check = duplicateDefinitions !> fmap show !> fromEither
