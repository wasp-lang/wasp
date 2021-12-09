{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Analyzer.Parser.ParseError
  ( ParseError (..),
    getErrorMessage,
    getSourcePosition,
  )
where

import Wasp.Analyzer.Parser.Token

data ParseError
  = -- | A lexical error representing an invalid character. It means that lexer
    -- failed to construct/parse a token due to this unexpected character.
    UnexpectedChar Char SourcePosition
  | -- | In @ParseError token expectedTokens@, @token@ is the token where parse error
    -- occured, while @expectedTokens@ is a list of tokens that would (any of them)
    -- avoid that error if they were there instead of the @token@.
    -- NOTE(martin): These @expectedTokens@ are represented via the names used for them
    --   in the grammar defined in Parser.y, under section @%token@ (names are in the
    --   first column), that have been a bit prettyfied (check Parser.y for details).
    UnexpectedToken Token [String]
  | -- | Thrown if parser encounters a quoter that has different tags, e.g.
    -- {=json psl=}. Then the first String in QuoterDifferentTags will be "json"
    -- while the second one will be "psl".
    QuoterDifferentTags (String, SourcePosition) (String, SourcePosition)
  deriving (Eq, Show)

getErrorMessage :: ParseError -> String
getErrorMessage (UnexpectedChar unexpectedChar _) =
  "Unexpected character: " ++ [unexpectedChar]
getErrorMessage (UnexpectedToken unexpectedToken expectedTokens) =
  unexpectedTokenMessage
    ++ if not (null expectedTokens) then "\n" ++ expectedTokensMessage else ""
  where
    unexpectedTokenMessage = "Unexpected token: " ++ tokenLexeme unexpectedToken
    expectedTokensMessage =
      "Expected one of the following tokens instead: "
        ++ unwords expectedTokens
getErrorMessage (QuoterDifferentTags (ltag, _) (rtag, _)) =
  "Quoter tags don't match: {=" ++ ltag ++ " ... " ++ rtag ++ "=}"

getSourcePosition :: ParseError -> SourcePosition
getSourcePosition (UnexpectedChar _ pos) = pos
getSourcePosition (UnexpectedToken Token {tokenPosition} _) = tokenPosition
getSourcePosition (QuoterDifferentTags _ (_, rpos)) = rpos
