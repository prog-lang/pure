module Utility.Pretty (printSection, printFileSection, attention) where

import Data.Char (toUpper)
import qualified System.Console.Terminal.Size as Console
import Utility.Fun ((|>))
import Utility.Strings (ul, (+-+), (+\\+))

printFileSection :: String -> String -> String -> String -> Int -> Int -> IO ()
printFileSection name source title message row column = printSection title body
  where
    body = "\n" ++ file +\\+ extract +\\+ message
    file = "in" +-+ name
    extract = attention source row column

printSection :: String -> String -> IO ()
printSection title body = do
  width <- consoleWidth
  putStrLn $ "\n" ++ br width (map toUpper title) +\\+ body ++ "\n"

br :: Int -> String -> String
br width_ message = if message == "" then line else left +-+ message +-+ right
  where
    line = take width_ dash
    left = take 3 dash
    right = take (width_ - length left - length message - 2) dash
    dash = repeat '-'

-- | Have to decide whether (row:column) are 0 or 1 indexed
attention :: String -> Int -> Int -> String
attention sourceText row column = "\n" ++ ul [line, tick]
  where
    line = ln ++ (sourceText |> lines |> (!! row))
    tick = repeat ' ' |> take (length ln + column) |> (++ "^")
    ln = show row +-+ "| "

consoleWidth :: IO Int
consoleWidth = do
  maybeSize <- Console.size
  let defaultWidth = 80
  return $ maybe defaultWidth Console.width maybeSize
