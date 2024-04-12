module CLI
  ( Application (name, version),
    Command (..),
    application,
    run,
    help,
    runIO,
  )
where

import Data.List (singleton)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Utility.Fun ((|>))
import Utility.Strings
  ( parenthesised,
    ul,
    (+-+),
    (+\+),
    (+\\+),
  )

-- TYPES -----------------------------------------------------------------------

data Application = Application
  { name :: String,
    version :: String,
    purpose :: String,
    authors :: [String],
    commands :: [Command],
    cmds :: Map String Command
  }

data Command
  = Command
  { longName :: String,
    shortName :: Maybe Char,
    description :: String,
    argCount :: Int,
    action :: Application -> [String] -> IO ()
  }

-- CONSTRUCT -------------------------------------------------------------------

application ::
  String ->
  String ->
  String ->
  [String] ->
  [Command] ->
  Application
application name' version' purpose' authors' commands' =
  let cmds' = commands' ++ [helpCommand, versionCommand]
   in Application
        { name = name',
          version = version',
          purpose = purpose',
          authors = authors',
          commands = cmds',
          cmds = Map.fromList $ concatMap namesAndActions cmds'
        }

helpCommand :: Command
helpCommand =
  Command
    { longName = "help",
      shortName = Nothing,
      description = "Display help message",
      argCount = 0,
      action = \app _ -> app |> help |> putStr
    }

versionCommand :: Command
versionCommand =
  Command
    { longName = "version",
      shortName = Nothing,
      description = "Display version info",
      argCount = 0,
      action = \app _ -> app |> overview |> putStrLn
    }

-- INSPECT COMMAND -------------------------------------------------------------

nameAndDescription :: Command -> String
nameAndDescription (Command long (Just short) hint _ _) =
  long +-+ parenthesised [short] +-+ "-" +-+ hint
nameAndDescription (Command long _ hint _ _) = long +-+ "-" +-+ hint

namesAndActions :: Command -> [(String, Command)]
namesAndActions command = (longName command, command) : shortOption
  where
    shortOption = maybe [] (\c -> [(singleton c, command)]) (shortName command)

-- INSPECT APPLICATION ---------------------------------------------------------

help :: Application -> String
help app =
  "OVERVIEW"
    +\+ ul [overview app]
    +\+ "COMMANDS"
    +\+ ul (map nameAndDescription (commands app))
    +\+ "AUTHORS"
    +\+ ul (authors app)

overview :: Application -> String
overview app = nameAndVersion app +-+ "-" +-+ purpose app

nameAndVersion :: Application -> String
nameAndVersion app = name app +-+ version app

-- RUN APPLICATION -------------------------------------------------------------

runIO :: Application -> IO ()
runIO app = getArgs >>= run app

run :: Application -> [String] -> IO ()
run app [] = defaultCommand app
run app (cmd : args)
  | commandMatches (length args) (Map.lookup cmd $ cmds app) =
      (action $ cmds app ! cmd) app args
run app _ = putStr $ unknownCommandSequence +\\+ help app

commandMatches :: Int -> Maybe Command -> Bool
commandMatches argc = maybe False ((argc ==) . argCount)

defaultCommand :: Application -> IO ()
defaultCommand = putStr . help

unknownCommandSequence :: String
unknownCommandSequence = "ERROR: UNKNOWN COMMAND SEQUENCE"
