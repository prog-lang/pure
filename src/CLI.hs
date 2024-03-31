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
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Strings
  ( parenthesised,
    ul,
    (+-+),
    (+\+),
    (+\\+),
  )
import System.Environment (getArgs)

-- TYPES -----------------------------------------------------------------------

data Application = Application
  { name :: String,
    version :: String,
    purpose :: String,
    authors :: [String],
    commands :: [Command],
    cmds :: Map String (Application -> IO ())
  }

data Command
  = Command
  { longName :: String,
    shortName :: Maybe Char,
    description :: String,
    action :: Application -> IO ()
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
      action = putStr . help
    }

versionCommand :: Command
versionCommand =
  Command
    { longName = "version",
      shortName = Nothing,
      description = "Display version info",
      action = putStrLn . overview
    }

-- INSPECT COMMAND -------------------------------------------------------------

nameAndDescription :: Command -> String
nameAndDescription (Command long (Just short) hint _) =
  long +-+ parenthesised [short] +-+ "-" +-+ hint
nameAndDescription (Command long _ hint _) = long +-+ "-" +-+ hint

namesAndActions :: Command -> [(String, Application -> IO ())]
namesAndActions command = (longName command, action command) : shortOption
  where
    shortOption = maybe [] (\c -> [(singleton c, action command)]) (shortName command)

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
run app [com] | Map.member com (cmds app) = (cmds app ! com) app
run app _ = putStr $ unknownCommandSequence +\\+ help app

defaultCommand :: Application -> IO ()
defaultCommand = putStr . help

unknownCommandSequence :: String
unknownCommandSequence = "ERROR: UNKNOWN COMMAND SEQUENCE"
