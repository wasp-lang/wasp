module Wasp.Psl.Parser.Tokens
  ( reserved,
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
  )
where

import Text.Megaparsec
  ( between,
    eof,
    many,
    manyTill,
    notFollowedBy,
    sepBy,
    sepBy1,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Wasp.Psl.Parser.Common (Parser)
import Wasp.Psl.Parser.Lexer (compulsoryNewline, lexeme, spaceConsumer)

reserved :: String -> Parser ()
reserved name =
  lexeme $
    try $
      do
        _ <- C.string name
        notFollowedBy identLetter <?> ("end of " ++ show name)

identifier :: Parser String
identifier = lexeme $ try ident
  where
    ident =
      do
        c <- identStart
        cs <- many identLetter
        return (c : cs)
        <?> "identifier"

identStart :: Parser Char
identStart = C.letterChar

identLetter :: Parser Char
identLetter = C.alphaNumChar <|> C.char '_'

braces :: Parser a -> Parser a
braces =
  between
    (symbol "{" >> compulsoryNewline)
    (symbol "}" >> (compulsoryNewline <|> eof))

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
symbol str = L.symbol spaceConsumer str

float :: Parser Double
float = L.signed spaceConsumer unsignedFloat
  where
    unsignedFloat = lexeme L.float

integer :: Parser Integer
integer = L.signed spaceConsumer unsignedInteger
  where
    unsignedInteger = lexeme L.decimal
