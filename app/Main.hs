module Main (main) where

import CLI (Application, Command (..), application, runIO)
import Data.Version (showVersion)
import Paths_purist (version)
import Pure.Parser (parseModule)
import qualified Pure.Typing.Check as Check
import qualified Pure.Typing.Error as TypingError
import Utility.Result (Result (..))
import Utility.Strings (ticked, (+-+))

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
          argCount = 1,
          action = compile
        }
    ]

compile :: Application -> [String] -> IO ()
compile _ [path] = readFile path >>= transpile path
compile _ _ = undefined

-- printOut :: (Show a, Show b) => Result a b -> IO ()
-- printOut (Ok ok) = print ok
-- printOut (Err err) = hPrint stderr err

transpile :: String -> String -> IO ()
transpile path input =
  case parseModule path input of
    Err parseError -> print parseError
    Ok parsedModule ->
      case Check.typing parsedModule of
        Err typingError -> mapM_ TypingError.printError typingError
        Ok _ -> putStrLn $ "Compiled" +-+ ticked path +-+ "successfully."
