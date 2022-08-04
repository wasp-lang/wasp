{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ParseError
  ( ParseError (..),
    getErrorMessageAndCtx,
  )
where

import Wasp.Analyzer.Parser.Ctx (Ctx, WithCtx (..), ctxFromPos, ctxFromRgn, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion, getRgnEnd, getRgnStart)
import Wasp.Analyzer.Parser.Token (Token (..), TokenKind)
import Wasp.Analyzer.Parser.TokenSet (TokenSet)

data ParseError
  = -- | @UnexpectedToken rgn errorKind expectedKinds@ is an error that occurs
    -- when one of @expectedKinds@ is expected, but the actual next token is
    -- @errorKind@.
    UnexpectedToken !SourceRegion !TokenKind TokenSet
  | -- | @UnexpectedEOF posn expectedKinds@ is an error that occurs when one of
    -- @expectedKinds@ is expected, but the input is empty.
    UnexpectedEOF !SourcePosition TokenSet
  | -- | Thrown if parser encounters a quoter that has different tags, e.g.
    -- {=json psl=}. Then the first String in QuoterDifferentTags will be "json"
    -- while the second one will be "psl".
    QuoterDifferentTags (WithCtx String) (WithCtx String)
  | -- | Thrown when the CST can not be coerced into an AST.
    --
    -- TODO: Add more specific variants instead of a generic catch-all error.
    ASTCoercionError String
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
