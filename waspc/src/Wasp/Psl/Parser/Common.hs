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
    SourceCode,
  )
where

import Text.Parsec
  ( alphaNum,
    char,
    letter,
    (<|>),
  )
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

type SourceCode = String

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

reserved :: String -> Parser ()
reserved = T.reserved lexer

identifier :: Parser String
identifier = T.identifier lexer

braces :: Parser a -> Parser a
braces = T.braces lexer

symbol :: String -> Parser String
symbol = T.symbol lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

stringLiteral :: Parser String
stringLiteral = T.stringLiteral lexer

brackets :: Parser a -> Parser a
brackets = T.brackets lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = T.commaSep1 lexer

commaSep :: Parser a -> Parser [a]
commaSep = T.commaSep lexer

colon :: Parser String
colon = T.colon lexer

float :: Parser Double
float = T.float lexer

integer :: Parser Integer
integer = T.integer lexer

lexer :: T.TokenParser ()
lexer =
  T.makeTokenParser
    emptyDef
      { T.commentLine = "//",
        T.caseSensitive = True,
        T.identStart = letter,
        T.identLetter = alphaNum <|> char '_'
      }
