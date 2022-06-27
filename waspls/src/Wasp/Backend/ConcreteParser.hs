module Wasp.Backend.ConcreteParser
  ( -- * Concrete parser

    -- | This module contains functions for converting lists of "Token"s into
    -- concrete syntax trees. These trees represent any source file completely,
    -- including whitespace and comments, even if the source file is invalid.
    parseCST,
    parseCSTExpression,
  )
where

import Wasp.Backend.ConcreteParser.Internal
import Wasp.Backend.ConcreteSyntax (SyntaxKind (..), SyntaxNode)
import Wasp.Backend.ParseError
import Wasp.Backend.Token (Token)
import qualified Wasp.Backend.Token as T

-- | Parse a list of tokens into a concrete syntax tree for an entire wasp file.
--
-- See "SyntaxNode" for a description of what a concrete syntax tree contains.
parseCST :: [Token] -> ([ParseError], [SyntaxNode])
parseCST tokens = parse tokens (root --> eof)

-- | Parse a list of tokens into a concrete syntax tree for a wasp expression.
-- This is mainly used for testing.
--
-- See "SyntaxNode" for a description of what a concrete syntax tree contains.
parseCSTExpression :: [Token] -> ([ParseError], [SyntaxNode])
parseCSTExpression tokens = parse tokens (expr --> eof)

root :: Parser
root = group Program stmts

stmts :: Parser
stmts = eof <> (stmt --> stmts)

stmt :: Parser
stmt =
  group Decl $
    (T.Identifier `as` DeclType)
      --> (T.Identifier `as` DeclName)
      --> expr

expr :: Parser
expr =
  group Dict (listLike lcurly dictEntry comma rcurly)
    <> group List (listLike lsquare expr comma rsquare)
    <> group Quoter (lquote --> quotedText)
    -- Note that we don't check number of tuple element here: this is left to
    -- the next phase of parsing.
    <> group Tuple (listLike lparen expr comma rparen)
    <> int
    <> double
    <> string
    <> (T.Identifier `as` Var)
    <> kwTrue
    <> kwFalse
    <> extImport

dictEntry :: Parser
dictEntry = group DictEntry $ (T.Identifier `as` DictKey) --> colon --> expr

quotedText :: Parser
quotedText = eof <> rquote <> (quoted --> quotedText)

extImport :: Parser
extImport =
  group ExtImport $
    kwImport --> extImportName --> kwFrom --> (T.String `as` ExtImportPath)

extImportName :: Parser
extImportName =
  (lcurly --> (T.Identifier `as` ExtImportField) --> rcurly)
    <> (T.Identifier `as` ExtImportModule)

-- | @listLike open value sep close@ parses list like structures in the form:
--
-- @open (value sep)* (value sep?) close@
--
-- In other words, a list with an optional trailing separator.
listLike ::
  -- | start parser
  Parser ->
  -- | value parser
  Parser ->
  -- | separator parser
  Parser ->
  -- | end parser
  Parser ->
  Parser
listLike open value sep close =
  open --> listLikeValues
  where
    listLikeValues :: Parser
    listLikeValues =
      eof
        <> close
        <> (perhaps sep --> ((value --> listLikeValues) <> close))
