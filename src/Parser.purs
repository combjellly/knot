module Parser where

import Prelude

import Text.Parsing.Parser 
import Text.Parsing.Parser.Pos
import Text.Parsing.Parser.Language (emptyDef)
import Text.Parsing.Parser.Token (GenLanguageDef(..),LanguageDef,unGenLanguageDef,TokenParser,GenTokenParser,makeTokenParser)
import Text.Parsing.Parser.Combinators
import Data.List.NonEmpty
import Data.Identity
import Effect.Console
import Data.Int (toNumber)
import Data.Array (many)
import Data.List
import Data.Either

import AST

-- show
{-
instance Show Element where
  show (Loop l) = show l
  show (Action a) = show a 
-}

type P a = ParserT String Identity a

-- Dictionary
tokenParser :: GenTokenParser String Identity
tokenParser = makeTokenParser $ LanguageDef (unGenLanguageDef emptyDef) {
  reservedNames = ["every","play"],
  reservedOpNames = [","]
  }

-- toNumber $ Int (CONVERTS INT TO NUMBER)

-- tokenParser
loop :: P Loop
loop = do
  reserved "every"
  n <- naturalOrFloat
  xs <- listOfActions
  pure $ Loop n xs

listOfActions :: P (List Action)
listOfActions = sepBy action (reservedOp ",")


intOrFloat ::P (Either Int Number)
intOrFloat = do
  x <- naturalOrFloat
  pure $ x



action :: P Action
action = do
  reserved "play"
  x <- identifier -- :: P String
  pure $ Action x


-- program
parse :: String -> Either ParseError Loop
parse x = do
    runParser x loop







-- Type Decleration

angles :: forall a. P a -> P a
angles = tokenParser.angles

braces :: forall a. P a -> P a
braces = tokenParser.braces

brackets :: forall a. P a -> P a
brackets = tokenParser.brackets

charLiteral :: P Char
charLiteral = tokenParser.charLiteral

colon :: P String
colon = tokenParser.colon

comma :: P String
comma = tokenParser.comma

commaSep :: forall a. P a -> P (List a)
commaSep = tokenParser.commaSep

commaSep1 :: forall a. P a -> P (NonEmptyList a)
commaSep1 = tokenParser.commaSep1

decimal :: P Int
decimal = tokenParser.decimal

dot :: P String
dot = tokenParser.dot

float :: P Number
float = tokenParser.float

hexadecimal :: P Int
hexadecimal = tokenParser.hexadecimal

identifier :: P String
identifier = tokenParser.identifier

integer :: P Int
integer = tokenParser.integer

lexeme :: forall a. P a -> P a
lexeme = tokenParser.lexeme

natural :: P Int
natural = tokenParser.natural

naturalOrFloat :: P (Either Int Number)
naturalOrFloat = tokenParser.naturalOrFloat

octal :: P Int
octal = tokenParser.octal

operator :: P String
operator = tokenParser.operator

parens :: forall a. P a -> P a
parens = tokenParser.parens

reserved :: String -> P Unit
reserved = tokenParser.reserved

reservedOp :: String -> P Unit
reservedOp = tokenParser.reservedOp

semi :: P String
semi = tokenParser.semi

semiSep :: forall a. P a -> P (List a)
semiSep = tokenParser.semiSep

semiSep1 :: forall a. P a -> P (NonEmptyList a)
semiSep1 = tokenParser.semiSep1

stringLiteral :: P String
stringLiteral = tokenParser.stringLiteral

symbol :: String -> P String
symbol = tokenParser.symbol

whiteSpace :: P Unit
whiteSpace = tokenParser.whiteSpace


