module Main (main) where

import CLI (Application, Command (..), application, runIO)
import Data.Version (showVersion)
import Paths_purist (version)
import Pure.Parser (Module, parseModule)
import System.IO (hPrint, stderr)
import Utility.Fun ((!>))
import Utility.Result (Result (..), (<!>))

main :: IO ()
main = runIO app

app :: Application
app =
  application
    "purist"
    (showVersion version)
    "Development suite for the Pure programming language"
    ["Viktor A. Rozenko Voitenko <sharp.vik@gmail.com>"]
    [ Command
        { longName = "compile",
          shortName = Just 'c',
          description = "Compile a single module",
          action = const compile
        }
    ]

compile :: IO ()
compile = getContents >>= transpile !> printOut

printOut :: (Show a, Show b) => Result a b -> IO ()
printOut (Ok ok) = print ok
printOut (Err err) = hPrint stderr err

transpile :: String -> Result String Module
transpile input = parseModule "main.pure" input <!> show
