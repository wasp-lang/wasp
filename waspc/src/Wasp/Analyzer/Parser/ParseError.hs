{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ParseError
  ( ParseError (..),
    getErrorMessageAndCtx,
  )
where

import Wasp.Analyzer.Parser.Ctx (Ctx, WithCtx (..), ctxFromPos, ctxFromRgn, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (getRgnEnd, getRgnStart)
import Wasp.Analyzer.Parser.Token (Token (..))

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
    QuoterDifferentTags (WithCtx String) (WithCtx String)
  deriving (Eq, Show)

getErrorMessageAndCtx :: ParseError -> (String, Ctx)
getErrorMessageAndCtx = \case
  UnexpectedChar unexpectedChar pos ->
    ( "Unexpected character: " ++ [unexpectedChar],
      ctxFromPos pos
    )
  UnexpectedToken unexpectedToken expectedTokens ->
    ( let unexpectedTokenMessage = "Unexpected token: " ++ tokenLexeme unexpectedToken
          expectedTokensMessage =
            "Expected one of the following tokens instead: "
              ++ unwords expectedTokens
       in unexpectedTokenMessage ++ if not (null expectedTokens) then "\n" ++ expectedTokensMessage else "",
      let tokenStartPos@(SourcePosition sl sc) = tokenStartPosition unexpectedToken
          tokenEndPos = SourcePosition sl (sc + length (tokenLexeme unexpectedToken) - 1)
       in ctxFromRgn tokenStartPos tokenEndPos
    )
  QuoterDifferentTags (WithCtx lctx ltag) (WithCtx rctx rtag) ->
    let ctx = ctxFromRgn (getRgnStart $ getCtxRgn lctx) (getRgnEnd $ getCtxRgn rctx)
     in ("Quoter tags don't match: {=" ++ ltag ++ " ... " ++ rtag ++ "=}", ctx)
