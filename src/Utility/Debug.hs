module Utility.Debug (hint) where

import Debug.Trace (trace)
import Utility.Strings ((+-+))

hint :: (Show a) => String -> a -> a
hint msg val = trace (msg +-+ show val) val