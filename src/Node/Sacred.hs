{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Node.Sacred where

import Data.Set (Set)
import qualified Data.Set as Set

-- KEYWORDS --------------------------------------------------------------------

keywords :: Set String
keywords =
  Set.fromList
    [ break_,
      case_,
      catch,
      class_,
      const_,
      continue,
      debugger,
      default_,
      delete,
      do_,
      else_,
      export,
      extends,
      false,
      finally,
      for,
      function,
      if_,
      import_,
      in_,
      instanceof,
      new,
      null_,
      return_,
      super,
      switch,
      this,
      throw,
      true,
      try,
      typeof,
      var,
      void,
      while,
      with,
      let_,
      static,
      yield,
      await,
      enum,
      implements,
      interface,
      package,
      private,
      protected,
      public,
      abstract,
      boolean,
      byte,
      char,
      double,
      final,
      float,
      goto,
      int,
      long,
      native,
      short,
      synchronized,
      throws,
      transient,
      volatile
    ]

break_ :: String
break_ = "break"

case_ :: String
case_ = "case"

catch :: String
catch = "catch"

class_ :: String
class_ = "class"

const_ :: String
const_ = "const"

continue :: String
continue = "continue"

debugger :: String
debugger = "debugger"

default_ :: String
default_ = "default"

delete :: String
delete = "delete"

do_ :: String
do_ = "do"

else_ :: String
else_ = "else"

export :: String
export = "export"

extends :: String
extends = "extends"

false :: String
false = "false"

finally :: String
finally = "finally"

for :: String
for = "for"

function :: String
function = "function"

if_ :: String
if_ = "if"

import_ :: String
import_ = "import"

in_ :: String
in_ = "in"

instanceof :: String
instanceof = "instanceof"

new :: String
new = "new"

null_ :: String
null_ = "null"

return_ :: String
return_ = "return"

super :: String
super = "super"

switch :: String
switch = "switch"

this :: String
this = "this"

throw :: String
throw = "throw"

true :: String
true = "true"

try :: String
try = "try"

typeof :: String
typeof = "typeof"

var :: String
var = "var"

void :: String
void = "void"

while :: String
while = "while"

with :: String
with = "with"

let_ :: String
let_ = "let"

static :: String
static = "static"

yield :: String
yield = "yield"

await :: String
await = "await"

enum :: String
enum = "enum"

implements :: String
implements = "implements"

interface :: String
interface = "interface"

package :: String
package = "package"

private :: String
private = "private"

protected :: String
protected = "protected"

public :: String
public = "public"

abstract :: String
abstract = "abstract"

boolean :: String
boolean = "boolean"

byte :: String
byte = "byte"

char :: String
char = "char"

double :: String
double = "double"

final :: String
final = "final"

float :: String
float = "float"

goto :: String
goto = "goto"

int :: String
int = "int"

long :: String
long = "long"

native :: String
native = "native"

short :: String
short = "short"

synchronized :: String
synchronized = "synchronized"

throws :: String
throws = "throws"

transient :: String
transient = "transient"

volatile :: String
volatile = "volatile"

exports :: String
exports = "module.exports"

-- SIGNS -----------------------------------------------------------------------

assign :: String
assign = "="

question :: String
question = "?"

comma :: String
comma = ","

colon :: String
colon = ":"

semicolon :: String
semicolon = ";"

lparen :: String
lparen = "("

rparen :: String
rparen = ")"

lbracket :: String
lbracket = "["

rbracket :: String
rbracket = "]"

lbrace :: String
lbrace = "{"

rbrace :: String
rbrace = "}"

arrow :: String
arrow = "=>"