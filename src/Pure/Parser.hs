{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pure.Parser
  ( Module (..),
    Id,
    moduleNames,
    assignmentNames,
    typeHintNames,
    typeDefNames,
    Definition (..),
    isTypeDef,
    defName,
    TypeHint (..),
    Expr (..),
    parseModule,
  )
where

import Data.Char (isLower)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe)
import qualified Pure.Sacred as S
import Text.Parsec
  ( ParseError,
    SourceName,
    alphaNum,
    between,
    char,
    endBy,
    eof,
    many,
    oneOf,
    optionMaybe,
    parse,
    sepBy,
    sepBy1,
    try,
    (<?>),
    (<|>),
  )
import Text.Parsec.Language (GenLanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token
  ( GenLanguageDef (..),
    GenTokenParser (..),
    makeTokenParser,
  )
import Utility.Common (Id)
import Utility.Fun ((!>))
import Utility.Result (Result)
import qualified Utility.Result as Result
import Utility.Strings (Parens (), commad, list, parenthesised, tuple, (+-+), (+\+))
import qualified Utility.Strings as Strings

-- TYPES -----------------------------------------------------------------------

data TypeHint
  = Func TypeHint TypeHint
  | Type Id [TypeHint]

data Module = Module
  { definitions :: [Definition],
    exports :: [Id]
  }

data Definition
  = ValueDef Id Expr -- main := 42;
  | TypeDef Id [Id] [TypeHint] -- type Maybe a is | Just a | Nothing;
  | TypeHint Id TypeHint -- main :: List Str -> IO Unit;

data Expr
  = Lam Id Expr
  | If Expr Expr Expr
  | App Expr [Expr]
  | List [Expr]
  | Id Id
  | Str String
  | Float Double
  | Int Integer
  | Bool Bool
  deriving (Eq)

-- INSPECT ---------------------------------------------------------------------

moduleNames :: Module -> [Id]
moduleNames (Module defs _) = map defName defs

assignmentNames :: Module -> [Id]
assignmentNames = map defName . filter isAssignment . definitions

typeHintNames :: Module -> [Id]
typeHintNames = map defName . filter isTypeHint . definitions

typeDefNames :: Module -> [Id]
typeDefNames = map defName . filter isTypeDef . definitions

defName :: Definition -> Id
defName (ValueDef name _) = name
defName (TypeDef name _ _) = name
defName (TypeHint name _) = name

isTypeDef :: Definition -> Bool
isTypeDef (TypeDef {}) = True
isTypeDef _ = False

isAssignment :: Definition -> Bool
isAssignment (ValueDef {}) = True
isAssignment _ = False

isTypeHint :: Definition -> Bool
isTypeHint (TypeHint _ _) = True
isTypeHint _ = False

-- SHOW ------------------------------------------------------------------------

instance Show Module where
  show (Module defs es) = unlines $ export : map show defs
    where
      export = S.export +-+ tuple es ++ S.str S.semicolon

instance Show Definition where
  show (ValueDef name expr) = name +-+ S.walrus +-+ show expr ++ S.str S.semicolon
  show (TypeDef name poly cons) =
    S.type_
      +-+ name
      +-+ unwords poly
      +-+ S.is
      +\+ unlines (map ((S.str S.bar +-+) . show) cons)
      ++ S.str S.semicolon
  show (TypeHint name ty) = name +-+ S.typed +-+ show ty ++ S.str S.semicolon

instance Parens TypeHint where
  parens this@(Type _ []) = show this
  parens t = parenthesised $ show t

instance Show TypeHint where
  show (Func a b) = show a +-+ S.arrow +-+ show b
  show (Type tag []) = tag
  show (Type tag args) = tag +-+ unwords (map Strings.parens args)

instance Show Expr where
  show (Bool bool) = show bool
  show (Int int) = show int
  show (Float number) = show number
  show (Str str) = show str
  show (Id ident) = ident
  show (List l) = list (map show l)
  show (App ex exs) = unwords $ map Strings.parens (ex : exs)
  show (If x y z) = S.if_ +-+ show x +-+ S.then_ +-+ show y +-+ S.else_ +-+ show z
  show (Lam p ex) = p +-+ S.arrow +-+ show ex

-- PARENS ----------------------------------------------------------------------

instance Parens Expr where
  parens i@(Int _) = show i
  parens f@(Float _) = show f
  parens s@(Str _) = show s
  parens i@(Id _) = show i
  parens l@(List _) = show l
  parens ex = parenthesised $ show ex

-- STATEMENT -------------------------------------------------------------------

data Statement = Export [Id] | Def Definition

unwrapExports :: Statement -> [Id]
unwrapExports (Export ids) = ids
unwrapExports _ = []

allExports :: [Statement] -> [Id]
allExports = concatMap unwrapExports

toDefinition :: Statement -> Maybe Definition
toDefinition (Def def) = Just def
toDefinition _ = Nothing

allDefinitions :: [Statement] -> [Definition]
allDefinitions = mapMaybe toDefinition

instance Show Statement where
  show (Export ids) = S.export +-+ parenthesised (commad ids) ++ S.str S.semicolon
  show (Def def) = show def

-- PARSER ----------------------------------------------------------------------

language :: (Monad m) => GenLanguageDef String u m
language =
  emptyDef
    { commentStart = "{-",
      commentEnd = "-}",
      commentLine = "--",
      nestedComments = True,
      identStart = identLetter language,
      identLetter = alphaNum <|> oneOf "_",
      opStart = opLetter language,
      opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~",
      reservedOpNames = S.operators,
      reservedNames = S.keywords,
      caseSensitive = True
    }

parser :: (Monad m) => GenTokenParser String u m
parser = makeTokenParser language

parseModule :: SourceName -> String -> Result ParseError Module
parseModule sourceName = parse moduleP sourceName !> Result.fromEither

moduleP :: Parser Module
moduleP = do
  ss <- statementsP
  return $ Module (allDefinitions ss) (allExports ss)

statementsP :: Parser [Statement]
statementsP = endBy statementP spacesP <* eof

statementP :: Parser Statement
statementP = (try exportP <|> definitionP) <* lexemeP (char S.semicolon)

exportP :: Parser Statement
exportP = Export <$> (reservedP S.export *> parensP (sepBy nameP $ lexemeP $ char S.comma))

definitionP :: Parser Statement
definitionP = Def <$> (typeDefP <|> try typeHintP <|> defP)

typeDefP :: Parser Definition
typeDefP = do
  _ <- reservedP S.type_
  name <- nameP
  params <- many nameP
  _ <- reservedP S.is >> barP
  cons <- sepBy1 typeConsP barP
  return $ TypeDef name params cons
  where
    barP = reservedOp parser [S.bar]

typeHintP :: Parser Definition
typeHintP = do
  name <- nameP
  _ <- reservedOp parser S.typed
  ty <- typeP
  return $ TypeHint name ty

typeConsP :: Parser TypeHint
typeConsP = do
  tag <- upperNameP
  params <- many typeLiteralP
  return $ Type tag params

typeP :: Parser TypeHint
typeP = try typeFunctionP <|> parensP typeP <|> taggedTypeP <?> "a type"

typeFunctionP :: Parser TypeHint
typeFunctionP = do
  t <- fromTypeP <* reservedOp parser S.arrow
  r <- typeP
  return $ Func t r

fromTypeP :: Parser TypeHint
fromTypeP = try taggedTypeP <|> typeLiteralP

taggedTypeP :: Parser TypeHint
taggedTypeP = do
  tag <- nameP
  params <- many typeLiteralP
  return $ Type tag params

typeLiteralP :: Parser TypeHint
typeLiteralP = try (parensP typeP) <|> justTagTypeP <?> "a type literal"

justTagTypeP :: Parser TypeHint
justTagTypeP = nameP <&> flip Type []

defP :: Parser Definition
defP = do
  name <- nameP
  _ <- reservedOp parser S.walrus
  expr <- exprP
  return $ ValueDef name expr

exprP :: Parser Expr
exprP = ifP <|> try lambdaP <|> try appP <|> literalP <?> "an expression"

ifP :: Parser Expr
ifP = do
  x <- between (reservedP S.if_) (reservedP S.then_) notIfP
  y <- notIfP <* reservedP S.else_
  z <- exprP
  return $ If x y z

-- | Any expression except `if` unless it's parenthesised.
notIfP :: Parser Expr
notIfP = try lambdaP <|> try appP <|> literalP <?> "a condition"

lambdaP :: Parser Expr
lambdaP = do
  p <- paramP <?> "a named parameter"
  expr <- exprP
  return $ Lam p expr
  where
    paramP :: Parser String
    paramP = nameP <* reservedOp parser S.arrow

appP :: Parser Expr
appP = do
  f <- callerP
  _ <- spacesP
  args <- sepBy1 literalP spacesP
  return $ App f args

callerP :: Parser Expr
callerP = try (parensP exprP) <|> try qualifiedP <|> idP

literalP :: Parser Expr
literalP =
  try (parensP exprP)
    <|> try listP
    <|> try boolP
    <|> try qualifiedP
    <|> try idP
    <|> try strP
    <|> try floatP
    <|> intP

listP :: Parser Expr
listP = brackets parser $ List <$> commaSep1 parser exprP

qualifiedP :: Parser Expr
qualifiedP = sepBy1 nameP (char S.dot) <&> intercalate (S.str S.dot) !> Id

idP :: Parser Expr
idP = Id <$> nameP

strP :: Parser Expr
strP = Str <$> stringLiteral parser

floatP :: Parser Expr
floatP = do
  sign <- optionMaybe $ char S.minus
  number <- float parser
  return $ Float $ if isJust sign then -number else number

intP :: Parser Expr
intP = Int <$> integer parser

boolP :: Parser Expr
boolP = (symbolP S.true <|> symbolP S.false) <&> read !> Bool

reservedP :: String -> Parser ()
reservedP = reserved parser

upperNameP :: Parser Id
upperNameP = do
  name <- nameP
  if isLower $ head name then fail "an uppercase identifier" else return name

nameP :: Parser Id
nameP = identifier parser

lexemeP :: Parser a -> Parser a
lexemeP = lexeme parser

spacesP :: Parser ()
spacesP = whiteSpace parser

parensP :: Parser a -> Parser a
parensP = parens parser

symbolP :: String -> Parser String
symbolP = symbol parser