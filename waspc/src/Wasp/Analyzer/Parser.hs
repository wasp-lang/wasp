module Wasp.Analyzer.Parser
  ( -- * Overview

    -- | The "Analyzer.Parser" module is built of two parts:
    --
    -- - The lexer, generated with Alex, which creates tokens from wasp source.
    -- - The parser, generated with Happy, which builds an abstract syntax
    --   tree from the tokens.
    --
    -- Lexing and parsing are not implemented as two separate phases that happen one after another.
    -- Instead, parser controls and uses lexer internally to produce tokens as needed, on the go.
    --
    -- Both lexer and parser are operating in a "Parser" monad, which manages state and exceptions for the parser,
    -- and therefore also for the lexer, which functions as a part of and is controlled by the parser.
    parseStatements,
    parseExpression,
    AST (..),
    Stmt (..),
    Expr (..),
    WithCtx (..),
    withCtx,
    ctxFromPos,
    ctxFromRgn,
    getCtxRgn,
    fromWithCtx,
    isValidWaspIdentifier,
    Ctx (..),
    Identifier,
    ExtImportName (..),
    ParseError (..),
    SourcePosition (..),
    Token (..),
    TokenKind (..),
  )
where

import Wasp.Analyzer.Parser.AST
import qualified Wasp.Analyzer.Parser.ConcreteParser as CST
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as CST
import qualified Wasp.Analyzer.Parser.ConcreteParser.ParseError as CST
import Wasp.Analyzer.Parser.Ctx (Ctx (..), WithCtx (..), ctxFromPos, ctxFromRgn, fromWithCtx, getCtxRgn, withCtx)
import qualified Wasp.Analyzer.Parser.Lexer as L
import Wasp.Analyzer.Parser.ParseError
import qualified Wasp.Analyzer.Parser.Parser as P
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.Token

-- | Checks if a string is a valid wasp identifier.
isValidWaspIdentifier :: String -> Bool
isValidWaspIdentifier str = case L.lex str of
  -- Lex the string and check that it only contains an Identifier token
  [tok] | tokenKind tok == Identifier -> True
  _ -> False

parseStatements :: String -> Either ParseError AST
parseStatements = runParser CST.parseCST P.parseStatements

parseExpression :: String -> Either ParseError Expr
parseExpression = runParser CST.parseCSTExpression P.parseExpression

-- TODO: report multiple errors. Have to hunt down everywhere this return is
-- used and figure out best way to handle list of ParseError
runParser ::
  ([Token] -> ([CST.ParseError], [CST.SyntaxNode])) ->
  (String -> [CST.SyntaxNode] -> Either ParseError a) ->
  (String -> Either ParseError a)
runParser cstParser astParser source =
  case cstParser (L.lex source) of
    ([], cst) -> astParser source cst
    (cstErrors, _) -> Left $ head $ map (parseErrorFromCSTParseError source) cstErrors
