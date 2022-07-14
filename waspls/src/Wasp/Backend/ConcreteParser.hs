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

root :: GrammarRule
root = Program <$$> stmts

stmts :: GrammarRule
stmts = eof <|> (stmt --> stmts)

stmt :: GrammarRule
stmt =
  Decl
    <$$> (T.Identifier `as` DeclType)
    --> (T.Identifier `as` DeclName)
    --> expr

{- ORMOLU_DISABLE -}
expr :: GrammarRule
expr =
        Dict <$$> listLike lcurly dictEntry comma rcurly
    <|> List <$$> listLike lsquare expr comma rsquare
    <|> Quoter <$$> (lquote --> quotedText)
    -- Note that we don't check number of tuple element here: this is left to
    -- the next phase of parsing.
    <|> Tuple <$$> listLike lparen expr comma rparen
    <|> int
    <|> double
    <|> string
    <|> (T.Identifier `as` Var)
    <|> kwTrue
    <|> kwFalse
    <|> extImport
{- ORMOLU_ENABLE -}

dictEntry :: GrammarRule
dictEntry = DictEntry <$$> (T.Identifier `as` DictKey) --> colon --> expr

quotedText :: GrammarRule
quotedText = rquote <|> (quoted --> quotedText)

extImport :: GrammarRule
extImport =
  ExtImport
    <$$> kwImport --> extImportName --> kwFrom --> (T.String `as` ExtImportPath)
  where
    extImportName :: GrammarRule
    extImportName =
      lcurly --> (T.Identifier `as` ExtImportField) --> rcurly
        <|> (T.Identifier `as` ExtImportModule)

-- | @listLike open value sep close@ parses list like structures in the form:
--
-- @open (value sep)* (value sep?) close@
--
-- In other words, a list with an optional trailing separator.
listLike ::
  -- | start grammar rule
  GrammarRule ->
  -- | value grammar rule
  GrammarRule ->
  -- | separator grammar rule
  GrammarRule ->
  -- | end grammar rule
  GrammarRule ->
  GrammarRule
listLike open value sep close =
  open --> listLikeValues
  where
    listLikeValues :: GrammarRule
    listLikeValues =
      close <|> (perhaps sep --> ((value --> listLikeValues) <|> close))
