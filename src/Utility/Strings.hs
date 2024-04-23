{-# OPTIONS_GHC -Wno-type-defaults #-}

module Utility.Strings
  ( commad,
    parenthesised,
    braced,
    ticked,
    bracketed,
    tab,
    ul,
    li,
    array,
    tuple,
    list,
    base26,
    trimSpaces,
    (>*),
    (+-+),
    (+\+),
    (+\\+),
    Parens (..),
  )
where

import Data.Char (chr, isSpace)
import Data.List (intercalate)

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
ul = unlines . map li . lines . unlines

li :: String -> String
li = (tab ++)

trimSpaces :: String -> String
trimSpaces = reverse . dropWhile isSpace . reverse

base26 :: (Integral a) => a -> String
base26 0 = "a"
base26 n = aux "" n
  where
    aux acc 0 = acc
    aux acc i =
      let (q, r) = i `divMod` 26
       in aux (chr (97 + fromIntegral r) : acc) q
