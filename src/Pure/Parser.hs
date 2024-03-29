{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pure.Parser (parseModule) where

import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe)
import Fun ((!>))
import Pure (Definition (..), Expr (..), Id, Module (..), Type (..))
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
statementP = (try exportP <|> definitionP) <* lexemeP (char S.semicolon)

exportP :: Parser Statement
exportP = Export <$> (reservedP S.export *> parensP (sepBy nameP $ lexemeP $ char S.comma))

definitionP :: Parser Statement
definitionP = Def <$> (typeDefP <|> defP)

typeDefP :: Parser Definition
typeDefP = do
  _ <- reservedP S.type_
  name <- nameP
  params <- many nameP
  _ <- reservedP S.is >> barP
  cons <- sepBy1 typeP barP
  return $ TypeDef name params cons
  where
    barP = reservedOp parser [S.bar]

typeP :: Parser Type
typeP = do
  tag <- nameP
  params <- many typeParamP
  return $ Type tag params

typeParamP :: Parser Type
typeParamP = try (parensP typeP) <|> (nameP <&> flip Type [])

defP :: Parser Definition
defP = do
  name <- nameP
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