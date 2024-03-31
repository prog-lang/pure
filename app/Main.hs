module Main (main) where

import CLI (Application, Command (..), application, runIO)
import Convert (Into (..))
import Data.Version (showVersion)
import Fun ((!>), (|>))
import qualified Node.Node as Node
import Node.Transpiler ()
import Paths_purist (version)
import Pure.Checks (duplicateDefinitions)
import Pure.Parser (parseModule)
import Result (Result (..), (<!>))
import System.IO (hPutStrLn, stderr)

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

printOut :: (Show a) => Result String a -> IO ()
printOut (Ok ok) = print ok
printOut (Err err) = hPutStrLn stderr err

transpile :: String -> Result String Node.Module
transpile input =
  parseModule "main.pure" input
    <!> show
    >>= duplicateDefinitions
      |> into
