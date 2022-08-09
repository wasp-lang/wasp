{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ParseError
  ( ParseError (..),
    parseErrorFromCSTParseError,
    getErrorMessageAndCtx,
  )
where

import qualified Wasp.Analyzer.Parser.ConcreteParser.ParseError as CST
import Wasp.Analyzer.Parser.Ctx (Ctx (Ctx), WithCtx (..), ctxFromPos, ctxFromRgn, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..), offsetToPosition)
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion), getRgnEnd, getRgnStart, offsetRegionToSourceRegion)
import Wasp.Analyzer.Parser.Token (TokenKind)
import Wasp.Analyzer.Parser.TokenSet (TokenSet)
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

data ParseError
  = -- | @UnexpectedToken region lexeme errorKind expectedKinds@ is an error that occurs
    -- when one of @expectedKinds@ is expected, but the actual next token is
    -- @errorKind@.
    UnexpectedToken !SourceRegion String !TokenKind TokenSet
  | -- | @UnexpectedEOF pos expectedKinds@ is an error that occurs when one of
    -- @expectedKinds@ is expected, but the input is empty.
    UnexpectedEOF !SourcePosition TokenSet
  | -- | Thrown if parser encounters a quoter that has different tags, e.g.
    -- {=json psl=}. Then the first String in QuoterDifferentTags will be "json"
    -- while the second one will be "psl".
    --
    -- TODO: This error is never actually used: the lexer will never produce a
    -- {= and =} next to each other with different tags.
    QuoterDifferentTags (WithCtx String) (WithCtx String)
  | -- | @TupleTooFewValues tupleRegion tupleSize@ occurs when a tuple contains
    -- less than the required two values.
    TupleTooFewValues !SourceRegion !Int
  | -- | @MissingSyntax pos expectedSyntax@ occurs when a piece of syntax is not
    -- found in the concrete parse tree. @expectedSyntax@ is a noun describing
    -- what type of syntax was expected. For example, if the source code is
    -- missing a comma after a dictionary entry, it would report @MissingSyntax
    -- _ "comma"@.
    MissingSyntax !SourcePosition String
  deriving (Eq, Show)

-- | @parseErrorFromCSTParseError source cstParseError@ creates a "ParseError"
-- that represents @cstParseError@, using @source@ to find the lexeme
-- representing the token where the error was produced.
parseErrorFromCSTParseError :: String -> CST.ParseError -> ParseError
parseErrorFromCSTParseError source (CST.UnexpectedToken (CST.Region start end) errorKind expected) =
  let rgn = offsetRegionToSourceRegion source (CST.Region start end)
      lexeme = take (end - start) $ drop start source
   in UnexpectedToken rgn lexeme errorKind expected
parseErrorFromCSTParseError source (CST.UnexpectedEOF offset expected) =
  let pos = offsetToPosition source offset
   in UnexpectedEOF pos expected

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
  TupleTooFewValues region actualLength ->
    ( "Tuple only contains " ++ show actualLength ++ " values, but it must contain at least 2 values",
      Ctx region
    )
  MissingSyntax pos expectedSyntax -> ("Missing expected " ++ expectedSyntax, ctxFromPos pos)
