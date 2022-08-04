{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ParseError
  ( ParseError (..),
    getErrorMessageAndCtx,
  )
where

import Wasp.Analyzer.Parser.Ctx (Ctx, WithCtx (..), ctxFromPos, ctxFromRgn, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion, getRgnEnd, getRgnStart)
import Wasp.Analyzer.Parser.Token (TokenKind)
import Wasp.Analyzer.Parser.TokenSet (TokenSet)
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

data ParseError
  = -- | @UnexpectedToken rgn lexeme errorKind expectedKinds@ is an error that occurs
    -- when one of @expectedKinds@ is expected, but the actual next token is
    -- @errorKind@.
    UnexpectedToken !SourceRegion !String !TokenKind TokenSet
  | -- | @UnexpectedEOF posn expectedKinds@ is an error that occurs when one of
    -- @expectedKinds@ is expected, but the input is empty.
    UnexpectedEOF !SourcePosition TokenSet
  | -- | Thrown if parser encounters a quoter that has different tags, e.g.
    -- {=json psl=}. Then the first String in QuoterDifferentTags will be "json"
    -- while the second one will be "psl".
    --
    -- TODO: This error is never actually used: the lexer will never produce a
    -- {= and =} next to each other with different tags.
    QuoterDifferentTags (WithCtx String) (WithCtx String)
  | -- | Thrown when the CST can not be coerced into an AST.
    --
    -- TODO: Add more specific variants instead of a generic catch-all error.
    ASTCoercionError !SourcePosition String
  deriving (Eq, Show)

getErrorMessageAndCtx :: ParseError -> (String, Ctx)
getErrorMessageAndCtx = \case
  UnexpectedToken rgn lexeme _ expectedTokens ->
    ( let unexpectedTokenMessage = "Unexpected token: " ++ lexeme
          expectedTokensMessage =
            "Expected one of the following tokens instead: "
              ++ TokenSet.showTokenSet expectedTokens
       in unexpectedTokenMessage ++ if not (TokenSet.null expectedTokens) then "\n" ++ expectedTokensMessage else "",
      ctxFromRgn (getRgnStart rgn) (getRgnEnd rgn)
    )
  UnexpectedEOF pos expectedTokens ->
    ( let unexpectedTokenMessage = "Unexpected end of file"
          expectedTokensMessage =
            "Expected one of the following tokens instead: "
              ++ TokenSet.showTokenSet expectedTokens
       in unexpectedTokenMessage ++ if not (TokenSet.null expectedTokens) then "\n" ++ expectedTokensMessage else "",
      ctxFromPos pos
    )
  QuoterDifferentTags (WithCtx lctx ltag) (WithCtx rctx rtag) ->
    let ctx = ctxFromRgn (getRgnStart $ getCtxRgn lctx) (getRgnEnd $ getCtxRgn rctx)
     in ("Quoter tags don't match: {=" ++ ltag ++ " ... " ++ rtag ++ "=}", ctx)
  ASTCoercionError pos message -> (message, ctxFromPos pos)
