module Main (main) where

import CLI (Application, Command (..), application, runIO)
import Data.Version (showVersion)
import qualified Node.Error as NodeError
import qualified Node.Transpiler as Node
import Paths_pure (version)
import Pure.Parser (parseModule)
import qualified Pure.Parser as Parser
import qualified Pure.Typing.Check as Check
import qualified Pure.Typing.Error as TypingError
import Pure.Typing.Module (Module)
import Utility.Result (Result (..))

main :: IO ()
main = runIO app

app :: Application
app =
  application
    "pure"
    (showVersion version)
    "Development suite for the Pure programming language"
    ["Viktor A. Rozenko Voitenko <sharp.vik@gmail.com>"]
    [ Command
        { longName = "compile",
          shortName = Just 'c',
          description = "Compile a single module",
          argCount = 1,
          action = compile
        }
    ]

compile :: Application -> [String] -> IO ()
compile _ [path] = readFile path >>= parse path
compile _ _ = undefined

parse :: String -> String -> IO ()
parse path input =
  case parseModule path input of
    Err parseError -> print parseError
    Ok parsedModule -> checkTypes parsedModule

checkTypes :: Parser.Module -> IO ()
checkTypes parsedModule =
  case Check.typing parsedModule of
    Err err -> mapM_ TypingError.printError err
    Ok modul -> codeGen modul

codeGen :: Module -> IO ()
codeGen modul =
  case Node.transpile modul of
    Err err -> NodeError.printError err
    Ok code -> print code
