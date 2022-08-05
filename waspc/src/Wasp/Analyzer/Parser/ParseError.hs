{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ParseError
  ( ParseError (..),
    ASTCoercionError (..),
    parseErrorFromCSTParseError,
    getErrorMessageAndCtx,
  )
where

import Wasp.Analyzer.Parser.ConcreteParser (SyntaxKind)
import qualified Wasp.Analyzer.Parser.ConcreteParser.ParseError as CST
import Wasp.Analyzer.Parser.Ctx (Ctx (Ctx), WithCtx (..), ctxFromPos, ctxFromRgn, getCtxRgn)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..), offsetToPosition)
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion), getRgnEnd, getRgnStart)
import Wasp.Analyzer.Parser.Token (TokenKind)
import Wasp.Analyzer.Parser.TokenSet (TokenSet)
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

data ParseError
  = -- | @UnexpectedToken rgn lexeme errorKind expectedKinds@ is an error that occurs
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
  | -- | Thrown when the CST can not be coerced into an AST.
    --
    -- TODO: Add more specific variants instead of a generic catch-all error.
    ASTCoercionError !SourcePosition ASTCoercionError
  deriving (Eq, Show)

data ASTCoercionError
  = -- | @UnexpectedNode unexpectedKind location@ is used when an invalid node
    -- type is encountered. This only happens when the CST parser produces
    -- incorrectly structured syntax trees, which is a bug, not user error.
    UnexpectedNode SyntaxKind String
  | MissingSyntax String
  deriving (Eq, Show)

-- | @parseErrorFromCSTParseError source cstParseError@ creates a "ParseError"
-- that represents @cstParseError@, using @source@ to find the lexeme
-- representing the token where the error was produced.
parseErrorFromCSTParseError :: String -> CST.ParseError -> ParseError
parseErrorFromCSTParseError source (CST.UnexpectedToken (CST.Region start end) errorKind expected) =
  let startPos = offsetToPosition source start
      endPos = offsetToPosition source end
      lexeme = take (end - start) $ drop start source
   in UnexpectedToken (SourceRegion startPos endPos) lexeme errorKind expected
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
  ASTCoercionError pos reason -> (getASTCoercionErrorMessage reason, ctxFromPos pos)

getASTCoercionErrorMessage :: ASTCoercionError -> String
getASTCoercionErrorMessage (UnexpectedNode unexpectedKind location) =
  "Unexpected syntax node " ++ show unexpectedKind ++ " " ++ location ++ ". Report this to the maintainers of wasp."
getASTCoercionErrorMessage (MissingSyntax description) =
  "Could not find expected " ++ description
