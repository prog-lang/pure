{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Pure.Parser
  ( Module (..),
    moduleNames,
    assignmentNames,
    typeHintNames,
    typeDefNames,
    Def (..),
    isTypeDef,
    defName,
    Expr (..),
    parseModule,
  )
where

import Data.Char (isLowerCase, isUpperCase)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (isJust, mapMaybe)
import Pure.Expr (Expr (..), positionOf)
import qualified Pure.Sacred as S
import Pure.Typing.Type (Type (..))
import Text.Parsec
  ( ParseError,
    ParsecT,
    SourceName,
    SourcePos,
    alphaNum,
    between,
    char,
    eof,
    getParserState,
    letter,
    many,
    many1,
    oneOf,
    optionMaybe,
    parse,
    sepBy,
    sepBy1,
    statePos,
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
import Utility.Strings (commad, parenthesised, tuple, (+-+), (+\+))

-- TYPES -----------------------------------------------------------------------

data Module = Module
  { definitions :: [Def],
    exports :: [Id]
  }

data Def
  = ValueDef Id Expr -- main := 42;
  | TypeDef Id [Id] [Type] -- type Maybe a is | Just a | Nothing;
  | TypeHint Id Type -- main :: List Str -> IO Unit;

-- INSPECT ---------------------------------------------------------------------

moduleNames :: Module -> [Id]
moduleNames (Module defs _) = map defName defs

assignmentNames :: Module -> [Id]
assignmentNames = map defName . filter isAssignment . definitions

typeHintNames :: Module -> [Id]
typeHintNames = map defName . filter isTypeHint . definitions

typeDefNames :: Module -> [Id]
typeDefNames = map defName . filter isTypeDef . definitions

defName :: Def -> Id
defName (ValueDef name _) = name
defName (TypeDef name _ _) = name
defName (TypeHint name _) = name

isTypeDef :: Def -> Bool
isTypeDef (TypeDef {}) = True
isTypeDef _ = False

isAssignment :: Def -> Bool
isAssignment (ValueDef {}) = True
isAssignment _ = False

isTypeHint :: Def -> Bool
isTypeHint (TypeHint _ _) = True
isTypeHint _ = False

-- SHOW ------------------------------------------------------------------------

instance Show Module where
  show (Module defs es) = unlines $ export : map show defs
    where
      export = S.export +-+ tuple es ++ [S.semicolon]

instance Show Def where
  show (ValueDef name expr) = name +-+ S.walrus +-+ show expr ++ [S.semicolon]
  show (TypeDef name poly cons) =
    S.type_
      +-+ name
      +-+ unwords poly
      +-+ S.is
      +\+ unlines (map (([S.bar] +-+) . show) cons)
      ++ [S.semicolon]
  show (TypeHint name ty) = name +-+ S.typed +-+ show ty ++ [S.semicolon]

-- STATEMENT -------------------------------------------------------------------

data Statement = Export [Id] | Def Def

unwrapExports :: Statement -> [Id]
unwrapExports (Export ids) = ids
unwrapExports _ = []

allExports :: [Statement] -> [Id]
allExports = concatMap unwrapExports

toDefinition :: Statement -> Maybe Def
toDefinition (Def def) = Just def
toDefinition _ = Nothing

allDefinitions :: [Statement] -> [Def]
allDefinitions = mapMaybe toDefinition

instance Show Statement where
  show (Export ids) = S.export +-+ parenthesised (commad ids) ++ [S.semicolon]
  show (Def def) = show def

-- PARSER ----------------------------------------------------------------------

language :: (Monad m) => GenLanguageDef String u m
language =
  emptyDef
    { commentStart = "{-",
      commentEnd = "-}",
      commentLine = "--",
      nestedComments = True,
      identStart = letter <|> char '_',
      identLetter = alphaNum <|> char '_',
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
statementsP = spacesP *> many statementP <* eof

statementP :: Parser Statement
statementP = (try exportP <|> definitionP) <* lexemeP (char S.semicolon)

exportP :: Parser Statement
exportP =
  reservedP S.export
    *> parensP (sepBy nameP $ lexemeP $ char S.comma)
    <&> Export

definitionP :: Parser Statement
definitionP = Def <$> (typeDefP <|> try typeHintP <|> defP)

typeDefP :: Parser Def
typeDefP = do
  name <- reservedP S.type_ >> upperNameP
  params <- many lowerNameP
  cons <- reservedP S.is >> barP >> sepBy1 typeConsP barP
  return $ TypeDef name params cons
  where
    barP = reservedOp parser [S.bar]

typeHintP :: Parser Def
typeHintP = do
  name <- nameP
  ty <- reservedOp parser S.typed >> typeP
  return $ TypeHint name ty

typeConsP :: Parser Type
typeConsP = do
  tag <- upperNameP
  params <- many typeLiteralP
  return $ Cons tag params

typeP :: Parser Type
typeP = try typeFunctionP <|> parensP typeP <|> taggedTypeP <?> "a type"

typeFunctionP :: Parser Type
typeFunctionP = do
  t <- fromTypeP <* reservedOp parser S.arrow
  r <- typeP
  return $ t :-> r

fromTypeP :: Parser Type
fromTypeP = try taggedTypeP <|> typeLiteralP

taggedTypeP :: Parser Type
taggedTypeP = do
  tag@(l : _) <- nameP
  params <- many typeLiteralP
  return $
    if null params
      then (if isLowerCase l then Var tag else Cons tag [])
      else Cons tag params

typeLiteralP :: Parser Type
typeLiteralP = try (parensP typeP) <|> typeVarP <?> "a type literal"

typeVarP :: Parser Type
typeVarP = do
  name@(l : _) <- nameP
  return $ if isLowerCase l then Var name else Cons name []

defP :: Parser Def
defP = do
  name <- nameP
  expr <- reservedOp parser S.walrus >> exprP
  return $ ValueDef name expr

exprP :: Parser Expr
exprP =
  -- try whenP
  try ifP
    <|> try lambdaP
    <|> try appP
    <|> literalP
    <?> "an expression"

-- whenP :: Parser Expr
-- whenP = do
--   pos <- sourcePos
--   e <- between (reservedP S.when) (reservedP S.is) notIfP
--   brs <- barP >> sepBy1 branchP barP
--   return $ When e brs pos
--   where
--     barP = reservedOp parser [S.bar]
--     branchP = do
--       pat <- litP <* reservedP S.then_
--       result <- exprP
--       return (pat, result)

ifP :: Parser Expr
ifP = do
  pos <- sourcePos
  x <- between (reservedP S.if_) (reservedP S.then_) notIfP
  y <- notIfP <* reservedP S.else_
  z <- exprP
  return $ If x y z pos

-- | Any expression except `if` unless it's parenthesised.
notIfP :: Parser Expr
notIfP = try lambdaP <|> try appP <|> literalP <?> "a condition"

lambdaP :: Parser Expr
lambdaP = do
  pos <- sourcePos
  param <- lowerNameP <* reservedOp parser S.arrow <?> "a named parameter"
  expr <- exprP
  return $ Lam param expr pos

appP :: Parser Expr
appP = do
  f <- callerP
  (x : xs) <- many1 literalP
  return $ foldl go (go f x) xs
  where
    go f x = App f x (positionOf f)

callerP :: Parser Expr
callerP = parensP exprP <|> try qualifiedP <|> idP

literalP :: Parser Expr
literalP = parensP exprP <|> litP

litP :: Parser Expr
litP =
  strP
    <|> try boolP
    <|> try qualifiedP
    <|> try idP
    <|> try floatP
    <|> intP

-- listP :: Parser Expr
-- listP = do
--   pos <- sourcePos
--   list <- brackets parser $ commaSep1 parser exprP
--   return $ List list pos

qualifiedP :: Parser Expr
qualifiedP = do
  pos <- sourcePos
  qual <- sepBy1 nameP (char S.dot) <&> intercalate [S.dot]
  return $ Id qual pos

idP :: Parser Expr
idP = do
  pos <- sourcePos
  name <- nameP
  return $ Id name pos

strP :: Parser Expr
strP = do
  pos <- sourcePos
  str <- stringLiteral parser
  return $ Str str pos

floatP :: Parser Expr
floatP = do
  pos <- sourcePos
  sign <- optionMaybe $ char S.minus
  number <- float parser
  return $ Float (if isJust sign then -number else number) pos

intP :: Parser Expr
intP = do
  pos <- sourcePos
  int <- integer parser
  return $ Int int pos

boolP :: Parser Expr
boolP = do
  pos <- sourcePos
  b <- symbolP S.true <|> symbolP S.false
  return $ Bool (read b) pos

reservedP :: String -> Parser ()
reservedP = reserved parser

upperNameP :: Parser Id
upperNameP = do
  name <- nameP
  if isLowerCase $ head name
    then fail "an uppercase identifier"
    else return name

lowerNameP :: Parser Id
lowerNameP = do
  name <- nameP
  if isUpperCase $ head name
    then fail "a lowercase identifier"
    else return name

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

sourcePos :: (Monad m) => ParsecT s u m SourcePos
sourcePos = statePos `fmap` getParserState