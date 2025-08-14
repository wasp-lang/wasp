module Wasp.Psl.Parser.Common
  ( whiteSpace,
    reserved,
    identifier,
    braces,
    symbol,
    parens,
    stringLiteral,
    brackets,
    commaSep1,
    commaSep,
    colon,
    float,
    integer,
    lexeme,
    SourceCode,
    Parser,
  )
where

import Control.Applicative (liftA2)
import Data.Functor (void)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    between,
    empty,
    many,
    manyTill,
    notFollowedBy,
    sepBy,
    sepBy1,
    takeWhileP,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void SourceCode

type SourceCode = String

reserved :: String -> Parser ()
reserved name =
  lexeme $ try $ C.string name >> (notFollowedBy identLetter <?> ("end of " ++ show name))

identifier :: Parser String
identifier =
  lexeme $ try (ident <?> "identifier")
  where
    ident = liftA2 (:) identHead identTail
    identHead = C.letterChar
    identTail = many identLetter

identLetter :: Parser Char
identLetter = C.alphaNumChar <|> C.char '_'

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

stringLiteral :: Parser String
stringLiteral = lexeme $ quote >> manyTill L.charLiteral quote
  where
    quote = C.char '"'

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

commaSep1 :: Parser a -> Parser [a]
commaSep1 = flip sepBy1 comma

commaSep :: Parser a -> Parser [a]
commaSep = flip sepBy comma

comma :: Parser String
comma = symbol ","

colon :: Parser String
colon = symbol ":"

symbol :: String -> Parser String
symbol = L.symbol whiteSpace

float :: Parser Double
float = L.signed whiteSpace unsignedFloat
  where
    unsignedFloat = lexeme L.float

integer :: Parser Integer
integer = L.signed whiteSpace unsignedInteger
  where
    unsignedInteger = lexeme L.decimal

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whiteSpace

whiteSpace :: Parser ()
whiteSpace =
  L.space (void C.spaceChar) (void lineComment) empty

lineComment :: Parser String
lineComment =
  try doubleSlashSymbol
    >> takeWhileP (Just "character") (/= '\n')
  where
    doubleSlashSymbol = C.string "//" >> notFollowedBy (C.char '/')
