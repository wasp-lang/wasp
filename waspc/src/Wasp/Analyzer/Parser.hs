module Wasp.Analyzer.Parser
  ( -- * Overview

    -- | The "Analyzer.Parser" module is built of three parts:
    --
    -- - The lexer, generated with Alex, which creates tokens from wasp source.
    -- - The concrete syntax parser, which takes tokens and builds a loosely
    --   structured concrete syntax tree. This step is included because it can
    --   handle syntax error recovery and allows for analysis on source that has
    --   errors in it (used in the language server).
    -- - The abstract syntax parser, which takes a concrete syntax tree and converts
    --   it into an abstract tree.
    --
    -- The phases are run in sequence, one after another.
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
    SourceRegion (..),
    Token (..),
    TokenKind (..),
  )
where

import Wasp.Analyzer.Parser.AST
import qualified Wasp.Analyzer.Parser.AbstractParser as P
import qualified Wasp.Analyzer.Parser.ConcreteParser as CST
import Wasp.Analyzer.Parser.Ctx (Ctx (..), WithCtx (..), ctxFromPos, ctxFromRgn, fromWithCtx, getCtxRgn, withCtx)
import qualified Wasp.Analyzer.Parser.Lexer as L
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (..))
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
