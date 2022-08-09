{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.AbstractParser
  ( -- * AST to CST conversion

    -- | This module takes @["SyntaxNode"]@ produced by 'Wasp.Analyzer.Parser.ConcreteParser'
    -- and converts it into an abstract syntax tree.
    parseStatements,
    parseExpression,
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad.Except (throwError)
import Control.Monad.State.Strict (gets)
import Data.Maybe (catMaybes)
import Wasp.Analyzer.Parser.AST (AST, Expr, ExtImportName, Identifier, Stmt)
import qualified Wasp.Analyzer.Parser.AST as AST
import Wasp.Analyzer.Parser.AbstractParser.Monad
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind, SyntaxNode (SyntaxNode))
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as S
import Wasp.Analyzer.Parser.Ctx (Ctx (Ctx), WithCtx (WithCtx), ctxFromRgn)
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion))
import qualified Wasp.Analyzer.Parser.Token as T

-- | An operation that turns some syntax nodes into a value and outputs the
-- remaining nodes.
type Action a = [SyntaxNode] -> ParserM (a, [SyntaxNode])

-- | @parseStatements sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST.
parseStatements :: String -> [SyntaxNode] -> Either ParseError AST
parseStatements source syntax = runParserM source $ coerceProgram syntax

-- | @parseExpression sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST representing a single expression. Currently, it allows extra
-- syntax after the single expression.
parseExpression :: String -> [SyntaxNode] -> Either ParseError Expr
parseExpression source syntax = case runParserM source $ coerceExpr syntax of
  Left err -> Left err
  Right (WithCtx _ expr, _) -> Right expr

-- | Try to turn CST into top-level AST.
coerceProgram :: [SyntaxNode] -> ParserM AST
coerceProgram [] = return $ AST.AST []
coerceProgram (SyntaxNode S.Program _ children : _) = AST.AST . catMaybes <$> mapM coerceStmt children
coerceProgram (SyntaxNode kind width _ : remaining)
  | S.syntaxKindIsTrivia kind = advance width >> coerceProgram remaining
  | otherwise = unexpectedNode kind "in root"

-- | Try to turn CST into Stmt AST. Returns @Nothing@ if the given node is a
-- trivia node.
coerceStmt :: SyntaxNode -> ParserM (Maybe (WithCtx Stmt))
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
  mapM_ (advance . S.snodeWidth) remaining
  return $ Just $ WithCtx (ctxFromRgn startPos endPos) (AST.Decl declType declName expr)
coerceStmt (SyntaxNode kind width _)
  | S.syntaxKindIsTrivia kind = advance width >> return Nothing
  | otherwise = unexpectedNode kind "in Program"

-- | Try to turn CST into Expr AST. Returns @(expr, remainingNodesFromInput)@
-- when successful.
coerceExpr :: Action (WithCtx Expr)
coerceExpr [] = throwMissingSyntax "expression"
coerceExpr (SyntaxNode kind width children : remaining)
  | S.syntaxKindIsTrivia kind = advance width >> coerceExpr remaining
  | otherwise = do
    startPos <- gets pstateStartPos
    expr <- case kind of
      S.String -> AST.StringLiteral . tail . init <$> consume width
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

coerceDict :: [SyntaxNode] -> ParserM Expr
coerceDict syntax = do
  (_, entries, _) <-
    runAction syntax $
      sequence3
        ( coerceLexeme (S.Token T.LCurly) "{",
          coerceEntries,
          coerceLexeme (S.Token T.RCurly) "}"
        )
  return $ AST.Dict entries

coerceEntries :: Action [(Identifier, WithCtx Expr)]
coerceEntries [] = return ([], [])
coerceEntries (n@(SyntaxNode kind width children) : remaining)
  | kind == S.DictEntry = (first . (:)) <$> coerceEntry children <*> coerceEntries remaining
  | kind == S.Token T.Comma || S.syntaxKindIsTrivia kind = advance width >> coerceEntries remaining
  | otherwise = return ([], n : remaining)

coerceEntry :: [SyntaxNode] -> ParserM (Identifier, WithCtx Expr)
coerceEntry syntax = do
  (dictKey, _, expr) <-
    runAction syntax $
      sequence3
        ( coerceLexeme S.DictKey "dictionary key",
          coerceLexeme (S.Token T.Colon) ":",
          coerceExpr
        )
  return (dictKey, expr)

coerceList :: [SyntaxNode] -> ParserM Expr
coerceList syntax = do
  (_, values, _) <-
    runAction syntax $
      sequence3
        ( coerceLexeme (S.Token T.LSquare) "[",
          coerceValues,
          coerceLexeme (S.Token T.RSquare) "]"
        )
  return $ AST.List values

coerceTuple :: [SyntaxNode] -> ParserM Expr
coerceTuple syntax = do
  startPos <- gets pstateStartPos
  (_, values, _) <-
    runAction syntax $
      sequence3
        ( coerceLexeme (S.Token T.LParen) "(",
          coerceValues,
          coerceLexeme (S.Token T.RParen) ")"
        )
  endPos <- gets pstateEndPos
  case values of
    (x1 : x2 : xs) -> return $ AST.Tuple (x1, x2, xs)
    _ -> throwError $ TupleTooFewValues (SourceRegion startPos endPos) (length values)

coerceValues :: Action [WithCtx Expr]
coerceValues [] = return ([], [])
coerceValues (n@(SyntaxNode kind width _) : remaining)
  | kind == S.Token T.Comma || S.syntaxKindIsTrivia kind = advance width >> coerceValues remaining
  | S.syntaxKindIsExpr kind = do
    expr <- runAction [n] coerceExpr
    first (expr :) <$> coerceValues remaining
  | otherwise = return ([], n : remaining)

coerceExtImport :: [SyntaxNode] -> ParserM Expr
coerceExtImport syntax = do
  (_, name, _, from) <-
    runAction syntax $
      sequence4
        ( coerceLexeme (S.Token T.KwImport) "import",
          coerceExtImportName,
          coerceLexeme (S.Token T.KwFrom) "from",
          coerceLexeme S.ExtImportPath "a string"
        )
  return $ AST.ExtImport name (tail $ init from)

coerceExtImportName :: Action ExtImportName
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

coerceQuoter :: [SyntaxNode] -> ParserM Expr
coerceQuoter syntax = do
  ((lquote, lquoteRgn), contents, (rquote, rquoteRgn)) <-
    runAction syntax $
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

collectQuoted :: Action [String]
collectQuoted [] = return ([], [])
collectQuoted (n@(SyntaxNode kind width _) : remaining)
  | kind == S.Token T.Quoted = do
    lexeme <- consume width
    first (lexeme :) <$> collectQuoted remaining
  | kind == S.Token T.RQuote = return ([], n : remaining)
  | otherwise = unexpectedNode kind "inside quoter"

-- | Run 2 actions, using the remaining nodes from each action for the next
sequence2 :: (Action a, Action b) -> Action (a, b)
sequence2 (fa, fb) syntax = do
  (a, syntax') <- fa syntax
  (b, syntax'') <- fb syntax'
  return ((a, b), syntax'')

-- | Run 3 actions, using the remaining nodes from each action for the next
sequence3 :: (Action a, Action b, Action c) -> Action (a, b, c)
sequence3 (fa, fb, fc) syntax = do
  (a, syntax') <- fa syntax
  (b, syntax'') <- fb syntax'
  (c, syntax''') <- fc syntax''
  return ((a, b, c), syntax''')

-- Run 4 actions, using the remaining nodes from each action for the next
sequence4 :: (Action a, Action b, Action c, Action d) -> Action (a, b, c, d)
sequence4 (fa, fb, fc, fd) syntax = do
  (a, syntax') <- fa syntax
  (b, syntax'') <- fb syntax'
  (c, syntax''') <- fc syntax''
  (d, syntax'''') <- fd syntax'''
  return ((a, b, c, d), syntax'''')

-- | Run an action and advance past all remaining nodes.
runAction :: [SyntaxNode] -> Action a -> ParserM a
runAction syntax fa = do
  (a, syntax') <- fa syntax
  mapM_ (advance . S.snodeWidth) syntax'
  return a

-- | Run an action and track the region surrounding it
withRegion :: Action a -> Action (a, SourceRegion)
withRegion fa syntax = do
  start <- gets pstateStartPos
  (a, syntax') <- fa syntax
  end <- gets pstateEndPos
  return ((a, SourceRegion start end), syntax')

coerceLexeme :: SyntaxKind -> String -> Action String
coerceLexeme _ description [] = throwMissingSyntax description
coerceLexeme wantedKind description (SyntaxNode kind width _ : remaining)
  | kind == wantedKind = do
    lexeme <- consume width
    return (lexeme, remaining)
  | S.syntaxKindIsTrivia kind = advance width >> coerceLexeme wantedKind description remaining
  | otherwise = unexpectedNode kind $ "instead of " ++ description

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
