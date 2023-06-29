module Wasp.Analyzer.Parser.AbstractParser
  ( -- * AST to CST conversion

    -- | This module takes @["SyntaxNode"]@ produced by 'Wasp.Analyzer.Parser.ConcreteParser'
    -- and converts it into an abstract syntax tree.
    parseStatements,
    parseExpression,
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad (join)
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (gets)
import Data.Maybe (catMaybes)
import Wasp.Analyzer.Parser.AST (AST, Expr, ExtImportName, Identifier, Stmt)
import qualified Wasp.Analyzer.Parser.AST as AST
import Wasp.Analyzer.Parser.AbstractParser.Monad
import Wasp.Analyzer.Parser.CST (SyntaxKind, SyntaxNode (SyntaxNode))
import qualified Wasp.Analyzer.Parser.CST as S
import Wasp.Analyzer.Parser.Ctx (Ctx (Ctx), WithCtx (WithCtx), ctxFromRgn)
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion))
import qualified Wasp.Analyzer.Parser.Token as T

-- TODO: This file could be further improved by refactoring it to make it more readable.
--   We covered this somewhat in the following GH issue: https://github.com/wasp-lang/wasp/issues/687 .

-- In the below type definitions, "using" a syntax node means that its width
-- is used to adjust the source position in the "ParseState".

-- | A parser that uses a single syntax node.
type NodeParser a = SyntaxNode -> ParserM a

-- | A parser that uses an entire list of syntax nodes.
type NodesParser a = [SyntaxNode] -> ParserM a

-- | A parser that may use only part of a list of syntax nodes. Returns the
-- nodes it does not use.
type NodesPartialParser a = [SyntaxNode] -> ParserM (a, [SyntaxNode])

-- | @parseStatements sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST.
parseStatements :: String -> [SyntaxNode] -> Either [ParseError] AST
parseStatements source syntax = runParserM source $ coerceProgram syntax

-- | @parseExpression sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST representing a single expression. It ignores any extra
-- syntax nodes after the expression. For example, for an input list of nodes
-- @[Integer, Whitespace, String]@ it would return an @IntLiteral@ AST node,
-- ignoring the @Whitespace@ and @String@ that follow.
--
-- This should never cause any issues: correct output from the CST parser will
-- not have any extra nodes (besides trivia nodes).
parseExpression :: String -> [SyntaxNode] -> Either [ParseError] Expr
parseExpression source syntax = case runParserM source $ coerceExpr syntax of
  Left errs -> Left errs
  Right (WithCtx _ expr, _) -> Right expr

-- | Try to turn CST into top-level 'AST'. Recovers from errors occuring inside
-- each statement node. Statements with errors are not included in the returned
-- 'AST'.
coerceProgram :: NodesParser AST
coerceProgram [] = return $ AST.AST []
coerceProgram (SyntaxNode S.Program _ children : _) =
  AST.AST . catMaybes
    <$> mapM (fmap join . nodeParserRecover coerceStmt) children
coerceProgram (SyntaxNode kind width _ : remaining)
  | S.syntaxKindIsTrivia kind = advance width >> coerceProgram remaining
  | otherwise = unexpectedNode kind "in root"

-- | Try to turn CST into Stmt AST. Returns @Nothing@ if the given node is a
-- trivia node. If parsing the node throws an error, the error is recovered from
-- and 'Nothing' is returned.
coerceStmt :: NodeParser (Maybe (WithCtx Stmt))
coerceStmt (SyntaxNode S.Decl _ children) = do
  startPos <- gets pstateStartPos
  ((declType, declName, expr), remaining) <-
    sequence3
      ( coerceLexeme S.DeclType "declaration type",
        coerceLexeme S.DeclName "declaration name",
        coerceExpr
      )
      children
  endPos <- gets pstateEndPos
  -- This is a node parser, so we have to use all remaining nodes
  mapM_ (advance . S.snodeWidth) remaining
  return $ Just $ WithCtx (ctxFromRgn startPos endPos) (AST.Decl declType declName expr)
coerceStmt (SyntaxNode kind width _)
  | S.syntaxKindIsTrivia kind = advance width >> return Nothing
  | otherwise = unexpectedNode kind "in Program"

-- | Try to turn CST into Expr AST. Returns @(expr, remainingNodesFromInput)@
-- when successful.
coerceExpr :: NodesPartialParser (WithCtx Expr)
coerceExpr [] = throwMissingSyntax "expression"
coerceExpr (SyntaxNode kind width children : remaining)
  | S.syntaxKindIsTrivia kind = advance width >> coerceExpr remaining
  | otherwise = do
      startPos <- gets pstateStartPos
      expr <- case kind of
        S.String -> AST.StringLiteral . read <$> consume width
        S.Int -> AST.IntegerLiteral . read <$> consume width
        S.Double -> AST.DoubleLiteral . read <$> consume width
        S.BoolTrue -> advance width >> return (AST.BoolLiteral True)
        S.BoolFalse -> advance width >> return (AST.BoolLiteral False)
        S.Var -> AST.Var <$> consume width
        S.Dict -> coerceDict children
        S.List -> coerceList children
        S.Tuple -> coerceTuple children
        S.ExtImport -> coerceExtImport children
        S.Quoter -> coerceQuoter children
        _ -> unexpectedNode kind "where an expression was expected"
      endPos <- gets pstateEndPos
      return (WithCtx (ctxFromRgn startPos endPos) expr, remaining)

coerceDict :: NodesParser Expr
coerceDict syntax = do
  (_, entries, _) <-
    runNodesPartialParser syntax $
      sequence3
        ( coerceLexeme (S.Token T.LCurly) "{",
          coerceEntries,
          coerceLexeme (S.Token T.RCurly) "}"
        )
  return $ AST.Dict entries

-- | Parse the comma-separated list of entries in a dictioanry. Recovers from
-- errors that occur within each entry. If an error occurs while parsing an
-- entry, that entry is not included in the returned list.
coerceEntries :: NodesPartialParser [(Identifier, WithCtx Expr)]
coerceEntries [] = return ([], [])
coerceEntries (n@(SyntaxNode kind width children) : remaining)
  | kind == S.DictEntry = first . mcons <$> coerceEntry children <*> coerceEntries remaining
  | kind == S.Token T.Comma || S.syntaxKindIsTrivia kind = advance width >> coerceEntries remaining
  | otherwise = return ([], n : remaining)

-- | Parse a dictionary entry. If parsing throws an error, the error is recovered
-- from and 'Nothing' is returned.
coerceEntry :: NodesParser (Maybe (Identifier, WithCtx Expr))
coerceEntry = nodesParserRecover $ \syntax -> do
  (dictKey, _, expr) <-
    runNodesPartialParser syntax $
      sequence3
        ( coerceLexeme S.DictKey "dictionary key",
          coerceLexeme (S.Token T.Colon) ":",
          coerceExpr
        )
  return (dictKey, expr)

coerceList :: NodesParser Expr
coerceList syntax = do
  (_, values, _) <-
    runNodesPartialParser syntax $
      sequence3
        ( coerceLexeme (S.Token T.LSquare) "[",
          coerceValues,
          coerceLexeme (S.Token T.RSquare) "]"
        )
  return $ AST.List values

coerceTuple :: NodesParser Expr
coerceTuple syntax = do
  startPos <- gets pstateStartPos
  (_, values, _) <-
    runNodesPartialParser syntax $
      sequence3
        ( coerceLexeme (S.Token T.LParen) "(",
          coerceValues,
          coerceLexeme (S.Token T.RParen) ")"
        )
  endPos <- gets pstateEndPos
  case values of
    (x1 : x2 : xs) -> return $ AST.Tuple (x1, x2, xs)
    _ -> throwError $ TupleTooFewValues (SourceRegion startPos endPos) (length values)

-- | Parse a list of comma-separated expressions. Recovers from errors within
-- each value in the list. Values with parse errors are not included in the
-- returned list of expressions.
coerceValues :: NodesPartialParser [WithCtx Expr]
coerceValues [] = return ([], [])
coerceValues (n@(SyntaxNode kind width _) : remaining)
  | kind == S.Token T.Comma || S.syntaxKindIsTrivia kind = advance width >> coerceValues remaining
  | S.syntaxKindIsExpr kind = do
      -- Recovers without consuming any nodes: this is ok because 'runNodesPartialParser'
      -- will consume all the nodes in that case.
      mbExpr <- runNodesPartialParser [n] $ nodesPartialParserRecoverAt (const True) coerceExpr
      first (mbExpr `mcons`) <$> coerceValues remaining
  | otherwise = return ([], n : remaining)

coerceExtImport :: NodesParser Expr
coerceExtImport syntax = do
  (_, name, _, from) <-
    runNodesPartialParser syntax $
      sequence4
        ( coerceLexeme (S.Token T.KwImport) "import",
          coerceExtImportName,
          coerceLexeme (S.Token T.KwFrom) "from",
          coerceLexeme S.ExtImportPath "a string"
        )
  return $ AST.ExtImport name (tail $ init from)

coerceExtImportName :: NodesPartialParser ExtImportName
coerceExtImportName [] = throwMissingSyntax "external import name"
coerceExtImportName (SyntaxNode kind width _ : remaining)
  | kind == S.ExtImportModule = do
      name <- AST.ExtImportModule <$> consume width
      return (name, remaining)
  | kind == S.Token T.LCurly = do
      advance width
      ((name, _), syntax') <-
        sequence2
          ( coerceLexeme S.ExtImportField "external import field",
            coerceLexeme (S.Token T.RCurly) "}"
          )
          remaining
      return (AST.ExtImportField name, syntax')
  | S.syntaxKindIsTrivia kind = advance width >> coerceExtImportName remaining
  | otherwise = unexpectedNode kind "in external import name"

coerceQuoter :: NodesParser Expr
coerceQuoter syntax = do
  ((lquote, lquoteRgn), contents, (rquote, rquoteRgn)) <-
    runNodesPartialParser syntax $
      sequence3
        ( withRegion $ coerceLexeme (S.Token T.LQuote) "{=tag",
          (first concat <$>) . collectQuoted,
          withRegion $ coerceLexeme (S.Token T.RQuote) "tag=}"
        )
  let ltag = drop 2 lquote
  let rtag = take (length rquote - 2) rquote
  if ltag /= rtag
    then
      throwError $
        QuoterDifferentTags
          (WithCtx (Ctx lquoteRgn) lquote)
          (WithCtx (Ctx rquoteRgn) rquote)
    else return $ AST.Quoter ltag contents

collectQuoted :: NodesPartialParser [String]
collectQuoted [] = return ([], [])
collectQuoted (n@(SyntaxNode kind width _) : remaining)
  | kind == S.Token T.Quoted = do
      lexeme <- consume width
      first (lexeme :) <$> collectQuoted remaining
  | kind == S.Token T.RQuote = return ([], n : remaining)
  | otherwise = unexpectedNode kind "inside quoter"

-- | Run 2 NodesPartialParsers, using the remaining nodes from each NodesPartialParser for the next
sequence2 :: (NodesPartialParser a, NodesPartialParser b) -> NodesPartialParser (a, b)
sequence2 (fa, fb) syntax = do
  (a, syntax') <- fa syntax
  (b, syntax'') <- fb syntax'
  return ((a, b), syntax'')

-- | Run 3 NodesPartialParsers, using the remaining nodes from each NodesPartialParser for the next
sequence3 :: (NodesPartialParser a, NodesPartialParser b, NodesPartialParser c) -> NodesPartialParser (a, b, c)
sequence3 (fa, fb, fc) syntax = do
  (a, syntax') <- fa syntax
  (b, syntax'') <- fb syntax'
  (c, syntax''') <- fc syntax''
  return ((a, b, c), syntax''')

-- Run 4 NodesPartialParsers, using the remaining nodes from each NodesPartialParser for the next
sequence4 :: (NodesPartialParser a, NodesPartialParser b, NodesPartialParser c, NodesPartialParser d) -> NodesPartialParser (a, b, c, d)
sequence4 (fa, fb, fc, fd) syntax = do
  (a, syntax') <- fa syntax
  (b, syntax'') <- fb syntax'
  (c, syntax''') <- fc syntax''
  (d, syntax'''') <- fd syntax'''
  return ((a, b, c, d), syntax'''')

-- | Run an NodesPartialParser and advance past all remaining nodes. Essentially,
-- this converts a 'NodesPartialParser' into a 'NodesParser'.
--
-- This function should only be used when, assuming we start with a valid CST,
-- there are only trivia nodes remaining.
runNodesPartialParser :: [SyntaxNode] -> NodesPartialParser a -> ParserM a
runNodesPartialParser syntax fa = do
  (a, syntax') <- fa syntax
  mapM_ (advance . S.snodeWidth) syntax'
  return a

-- | Run an NodesPartialParser and track the region surrounding it
withRegion :: NodesPartialParser a -> NodesPartialParser (a, SourceRegion)
withRegion fa syntax = do
  start <- gets pstateStartPos
  (a, syntax') <- fa syntax
  end <- gets pstateEndPos
  return ((a, SourceRegion start end), syntax')

coerceLexeme :: SyntaxKind -> String -> NodesPartialParser String
coerceLexeme _ description [] = throwMissingSyntax description
coerceLexeme wantedKind description (SyntaxNode kind width _ : remaining)
  | kind == wantedKind = do
      lexeme <- consume width
      return (lexeme, remaining)
  | S.syntaxKindIsTrivia kind = advance width >> coerceLexeme wantedKind description remaining
  | otherwise = unexpectedNode kind $ "instead of " ++ description

-- | Try running a 'NodeParser'. If the parser throws an error, reset state to
-- before the parser was ran, use the node, and return 'Nothing'.
nodeParserRecover :: NodeParser a -> NodeParser (Maybe a)
nodeParserRecover parser node =
  try (parser node) >>= \case
    Right a -> return $ Just a
    Left _ -> advance (S.snodeWidth node) >> return Nothing

-- | Try running a 'NodesParser'. If the parser throws an error, reset state to
-- before the parser was ran, use all of the nodes, and return 'Nothing'.
nodesParserRecover :: NodesParser a -> NodesParser (Maybe a)
nodesParserRecover parser nodes =
  try (parser nodes) >>= \case
    Right a -> return $ Just a
    Left _ -> advance (sum $ map S.snodeWidth nodes) >> return Nothing

-- | Try running a 'NodesPartialParser'. If the parser throws an error, reset state to
-- before the parser was ran, and use the nodes until the given condition is true
-- on the next unused node.
nodesPartialParserRecoverAt :: (SyntaxKind -> Bool) -> NodesPartialParser a -> NodesPartialParser (Maybe a)
nodesPartialParserRecoverAt cond parser nodes =
  try (parser nodes) >>= \case
    Right (a, remaining) -> return (Just a, remaining)
    Left _ -> do
      let (use, remaining) = break (cond . S.snodeKind) nodes
      advance $ sum $ map S.snodeWidth use
      return (Nothing, remaining)

-- | @mbX `mcons` xs@ conses a value to a list if the value exists.
mcons :: Maybe a -> [a] -> [a]
mcons (Just x) xs = x : xs
mcons Nothing xs = xs

-- | Alias for 'pstatePos'
pstateStartPos :: ParseState -> SourcePosition
pstateStartPos = pstatePos

-- | Get the position of the previous character. This can be used for getting
-- the (inclusive) end position for a region.
pstateEndPos :: ParseState -> SourcePosition
pstateEndPos state = case pstatePos state of
  -- Move back a character, moving up a line if necessary
  SourcePosition 1 1 -> SourcePosition 1 1
  SourcePosition l 1 -> SourcePosition (l - 1) (pstateLastLineLength state)
  SourcePosition l c -> SourcePosition l (c - 1)
