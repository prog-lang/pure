module Utility.Pretty (printSection) where

import Data.Char (toUpper)
import System.Console.Terminal.Size (Window (..))
import qualified System.Console.Terminal.Size as Console
import Utility.Strings ((+-+), (+\\+))

printSection :: String -> String -> IO ()
printSection title body = do
  maybeSize <- Console.size
  let defaultWidth = 80
  let w = maybe defaultWidth width maybeSize
  let capTitle = map toUpper title
  putStrLn $ "\n" ++ br w capTitle +\\+ body ++ "\n"

br :: Int -> String -> String
br width_ message = if message == "" then line else left +-+ message +-+ right
  where
    line = take width_ dash
    left = take 3 dash
    right = take (width_ - length left - length message - 2) dash
    dash = repeat '-'