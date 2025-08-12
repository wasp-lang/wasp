module Wasp.Psl.Parser.Lexer
  ( compulsoryNewline,
    lexeme,
    spaceConsumer,
    spaceConsumerNL,
  )
where

import qualified Data.Char as Char
import Data.Functor (void)
import Text.Megaparsec
  ( MonadParsec (label),
    empty,
    notFollowedBy,
    satisfy,
    takeWhileP,
    try,
    (<?>),
    (<|>),
  )
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Wasp.Psl.Parser.Common (Parser)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

compulsoryNewline :: Parser ()
compulsoryNewline = newline >> spaceConsumerNL

spaceConsumerNL :: Parser ()
spaceConsumerNL =
  L.space
    whiteSpaceNL
    (void lineComment)
    empty

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    whiteSpace
    lineComment
    empty

lineComment :: Parser ()
lineComment =
  void $
    label "line comment" $
      try commentSymbol
        >> takeWhileP (Just "character") (/= '\n')
  where
    commentSymbol = C.string "//" >> notFollowedBy newline

whiteSpaceNL :: Parser ()
whiteSpaceNL = whiteSpace <|> newline

newline :: Parser ()
newline = void C.newline <|> void C.crlf

whiteSpace :: Parser ()
whiteSpace = void (satisfy isNonNewlineSpace <?> "white space")
  where
    isNonNewlineSpace c = Char.Space == Char.generalCategory c
