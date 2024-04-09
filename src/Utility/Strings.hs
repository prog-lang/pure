{-# OPTIONS_GHC -Wno-type-defaults #-}

module Utility.Strings
  ( commad,
    parenthesised,
    braced,
    ticked,
    bracketed,
    tab,
    ul,
    array,
    tuple,
    list,
    numbered,
    trimSpaces,
    (>*),
    (+-+),
    (+\+),
    (+\\+),
    Parens (..),
  )
where

import Data.Char (isSpace)
import Data.List (intercalate)
import Utility.Fun ((!>))

class Parens a where
  parens :: a -> String

infixl 6 >*

(>*) :: String -> String -> String
wrapper >* target = wrapper ++ target ++ wrapper

infixr 5 +-+

(+-+) :: String -> String -> String
x +-+ y = x ++ " " ++ y

infixr 5 +\+

(+\+) :: String -> String -> String
x +\+ y = x ++ "\n" ++ y

infixr 5 +\\+

(+\\+) :: String -> String -> String
x +\\+ y = x ++ "\n\n" ++ y

ticked :: String -> String
ticked it = "`" ++ it ++ "`"

commad :: [String] -> String
commad = intercalate ", "

parenthesised :: String -> String
parenthesised a = "(" ++ a ++ ")"

braced :: String -> String
braced a = "{" ++ a ++ "}"

bracketed :: String -> String
bracketed a = "[" ++ a ++ "]"

tuple :: [String] -> String
tuple = parenthesised . commad

array :: [String] -> String
array = braced . commad

list :: [String] -> String
list = bracketed . commad

tab :: String
tab = replicate 4 ' '

ul :: [String] -> String
ul = unlines . map li

li :: String -> String
li = (tab ++)

trimSpaces :: String -> String
trimSpaces = reverse . dropWhile isSpace . reverse

numbered :: [a] -> [String]
numbered = length !> flip take [1 ..] !> map show !> map ("_" ++)
