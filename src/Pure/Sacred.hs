{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Pure.Sacred where

import qualified Data.List as List

-- KEYWORDS --------------------------------------------------------------------

isKeyword :: String -> Bool
isKeyword = flip List.elem keywords

keywords :: [String]
keywords = [export, if_, then_, else_, type_, is]

export :: String
export = "export"

if_ :: String
if_ = "if"

then_ :: String
then_ = "then"

else_ :: String
else_ = "else"

type_ :: String
type_ = "type"

is :: String
is = "is"

true :: String
true = show True

false :: String
false = show False

-- SIGNS -----------------------------------------------------------------------

operators :: [String]
operators = [walrus, arrow, typed, [minus]]

walrus :: String
walrus = ":="

typed :: String
typed = "::"

arrow :: String
arrow = "->"

semicolon :: Char
semicolon = ';'

lparen :: String
lparen = "("

rparen :: String
rparen = ")"

lbracket :: String
lbracket = "["

rbracket :: String
rbracket = "]"

underscore :: Char
underscore = '_'

dot :: Char
dot = '.'

comma :: Char
comma = ','

minus :: Char
minus = '-'

bar :: Char
bar = '|'
