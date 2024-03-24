{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-
SOME DOCS:
- https://wiki.haskell.org/Parsing_expressions_and_statements
- https://hackage.haskell.org/package/parsec-3.1.16.1/docs/src/Text.Parsec.Language.html#haskellDef
- https://hackage.haskell.org/package/parsec-3.1.16.1/docs/src/Text.Parsec.Token.html#GenTokenParser
-}

module Pure.Parser (parseModule) where

import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe)
import Fun ((!>))
import Pure (Definition (..), Expr (..), Id, Module (..))
import qualified Pure.Sacred as S
import Result (Result)
import qualified Result
import Strings (commad, parenthesised, (+-+))
import Text.Parsec
  ( ParseError,
    SourceName,
    alphaNum,
    between,
    char,
    endBy,
    eof,
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

-- STATEMENT

data Statement = Export [Id] | Def Definition

unwrapExports :: Statement -> [Id]
unwrapExports (Export ids) = ids
unwrapExports _ = []

allExports :: [Statement] -> [Id]
allExports = concat . map unwrapExports

toDefinition :: Statement -> Maybe Definition
toDefinition (Def def) = Just def
toDefinition _ = Nothing

allDefinitions :: [Statement] -> [Definition]
allDefinitions = mapMaybe toDefinition

instance Show Statement where
  show (Export ids) = S.export +-+ parenthesised (commad ids) ++ S.str S.semicolon
  show (Def def) = show def

-- PARSER

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
statementP = options <* lexemeP (char S.semicolon)
  where
    options = try (Export <$> exportP) <|> Def <$> defP

exportP :: Parser [Id]
exportP = do
  _ <- reservedP S.export
  parensP $ sepBy nameP $ lexemeP $ char S.comma

defP :: Parser Definition
defP = do
  name <- lexemeP nameP
  _ <- reservedOp parser S.walrus
  expr <- lexemeP exprP
  return $ name := expr

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
  p <- param <?> "a named parameter"
  expr <- exprP
  return $ Lam p expr
  where
    param :: Parser String
    param = do
      n <- nameP
      _ <- spacesP >> reservedOp parser S.arrow >> spacesP
      return n

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
    <|> try qualifiedP
    <|> try idP
    <|> try strP
    <|> try floatP
    <|> try intP

listP :: Parser Expr
listP = brackets parser $ List <$> commaSep1 parser exprP

qualifiedP :: Parser Expr
qualifiedP = do
  parts <- sepBy1 nameP (char S.dot)
  return $ Id $ intercalate (S.str S.dot) parts

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

reservedP :: String -> Parser ()
reservedP = reserved parser

nameP :: Parser Id
nameP = identifier parser

lexemeP :: Parser a -> Parser a
lexemeP = lexeme parser

spacesP :: Parser ()
spacesP = whiteSpace parser

parensP :: Parser a -> Parser a
parensP = parens parser