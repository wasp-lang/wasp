{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.Parser
  ( -- * AST to CST conversion

    -- | This module takes @["SyntaxNode"]@ produced by 'Wasp.Analyzer.Parser.ConcreteParser'
    -- and converts it into an abstract syntax tree.
    parseStatements,
    parseExpression,
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Maybe (catMaybes)
import Wasp.Analyzer.Parser.AST (AST, Expr, ExtImportName, Identifier, Stmt)
import qualified Wasp.Analyzer.Parser.AST as AST
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind, SyntaxNode (SyntaxNode))
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as S
import Wasp.Analyzer.Parser.Ctx (Ctx (Ctx), WithCtx (WithCtx), ctxFromRgn)
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))
import Wasp.Analyzer.Parser.SourceRegion (SourceRegion (SourceRegion))
import qualified Wasp.Analyzer.Parser.Token as T

data ParseState = ParseState
  { pstateLine :: !Int,
    pstateColumn :: !Int,
    pstateRemainingSource :: String
  }

pstatePos :: ParseState -> SourcePosition
pstatePos s = SourcePosition (pstateLine s) (pstateColumn s)

type ParserM a = StateT ParseState (Except ParseError) a

-- | An operation that turns some syntax nodes into a value and outputs the
-- remaining nodes.
type Action a = [SyntaxNode] -> ParserM (a, [SyntaxNode])

-- | @parseStatements sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST.
parseStatements :: String -> [SyntaxNode] -> Either ParseError AST
parseStatements source syntax = runExcept $ evalStateT (coerceProgram syntax) initialState
  where
    initialState =
      ParseState
        { pstateLine = 1,
          pstateColumn = 1,
          pstateRemainingSource = source
        }

-- | @parseExpression sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST representing a single expression. Currently, it allows extra
-- syntax after the single expression.
parseExpression :: String -> [SyntaxNode] -> Either ParseError Expr
parseExpression source syntax = case runExcept $ evalStateT (coerceExpr syntax) initialState of
  Left err -> Left err
  Right (WithCtx _ expr, _) -> Right expr
  where
    initialState =
      ParseState
        { pstateLine = 1,
          pstateColumn = 1,
          pstateRemainingSource = source
        }

-- | Try to turn CST into top-level AST.
coerceProgram :: [SyntaxNode] -> ParserM AST
coerceProgram [] = return $ AST.AST []
coerceProgram (SyntaxNode S.Program _ children : _) = AST.AST . catMaybes <$> mapM coerceStmt children
coerceProgram (SyntaxNode k w _ : ns)
  | S.syntaxKindIsTrivia k = advance w >> coerceProgram ns
  | otherwise = failParse $ UnexpectedNode k "in root"

-- | Try to turn CST into Stmt AST. Returns @Nothing@ if the given node is a
-- trivia node.
coerceStmt :: SyntaxNode -> ParserM (Maybe (WithCtx Stmt))
coerceStmt (SyntaxNode S.Decl _ children) = do
  startPos <- gets pstatePos
  ((declType, declName, expr), remaining) <-
    sequence3
      ( coerceLexeme S.DeclType "declaration type",
        coerceLexeme S.DeclName "declaration name",
        coerceExpr
      )
      children
  endPos <- gets pstatePos
  mapM_ (advance . S.snodeWidth) remaining
  return $ Just $ WithCtx (ctxFromRgn startPos endPos) (AST.Decl declType declName expr)
coerceStmt (SyntaxNode k w _)
  | S.syntaxKindIsTrivia k = advance w >> return Nothing
  | otherwise = failParse $ UnexpectedNode k "in Program"

-- | Try to turn CST into Expr AST. Returns @(expr, remainingNodesFromInput)@
-- when successful.
coerceExpr :: Action (WithCtx Expr)
coerceExpr [] = failParse $ MissingSyntax "expression"
coerceExpr (SyntaxNode k w children : ns)
  | S.syntaxKindIsTrivia k = advance w >> coerceExpr ns
  | otherwise = do
    startPos <- gets pstatePos
    expr <- case k of
      S.String -> AST.StringLiteral . tail . init <$> consume w
      S.Int -> AST.IntegerLiteral . read <$> consume w
      S.Double -> AST.DoubleLiteral . read <$> consume w
      S.BoolTrue -> advance w >> return (AST.BoolLiteral True)
      S.BoolFalse -> advance w >> return (AST.BoolLiteral False)
      S.Var -> AST.Var <$> consume w
      S.Dict -> coerceDict children
      S.List -> coerceList children
      S.Tuple -> coerceTuple children
      S.ExtImport -> coerceExtImport children
      S.Quoter -> coerceQuoter children
      _ -> failParse $ UnexpectedNode k "where an expression was expected"
    endPos <- gets pstatePos
    return (WithCtx (ctxFromRgn startPos endPos) expr, ns)

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
coerceEntries (n@(SyntaxNode k w children) : ns)
  | k == S.DictEntry = (first . (:)) <$> coerceEntry children <*> coerceEntries ns
  | k == S.Token T.Comma || S.syntaxKindIsTrivia k = advance w >> coerceEntries ns
  | otherwise = return ([], n : ns)

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
  startPos <- gets pstatePos
  (_, values, _) <-
    runAction syntax $
      sequence3
        ( coerceLexeme (S.Token T.LParen) "(",
          coerceValues,
          coerceLexeme (S.Token T.RParen) ")"
        )
  endPos <- gets pstatePos
  case values of
    (x1 : x2 : xs) -> return $ AST.Tuple (x1, x2, xs)
    _ -> throwError $ TupleTooFewValues (SourceRegion startPos endPos) (length values)

coerceValues :: Action [WithCtx Expr]
coerceValues [] = return ([], [])
coerceValues (n@(SyntaxNode k w _) : ns)
  | k == S.Token T.Comma || S.syntaxKindIsTrivia k = advance w >> coerceValues ns
  | S.syntaxKindIsExpr k = do
    expr <- runAction [n] coerceExpr
    first (expr :) <$> coerceValues ns
  | otherwise = return ([], n : ns)

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
coerceExtImportName [] = failParse $ MissingSyntax "external import name"
coerceExtImportName (SyntaxNode k w _ : ns)
  | k == S.ExtImportModule = do
    name <- AST.ExtImportModule <$> consume w
    return (name, ns)
  | k == S.Token T.LCurly = do
    advance w
    ((name, _), syntax') <-
      sequence2
        ( coerceLexeme S.ExtImportField "external import field",
          coerceLexeme (S.Token T.RCurly) "}"
        )
        ns
    return (AST.ExtImportField name, syntax')
  | S.syntaxKindIsTrivia k = advance w >> coerceExtImportName ns
  | otherwise = failParse $ UnexpectedNode k "in external import name"

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
collectQuoted (n@(SyntaxNode k w _) : ns)
  | k == S.Token T.Quoted = do
    lexeme <- consume w
    (lexemes, remaining) <- collectQuoted ns
    return (lexeme : lexemes, remaining)
  | k == S.Token T.RQuote = return ([], n : ns)
  | otherwise = failParse $ UnexpectedNode k "inside quoter"

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
  start <- gets pstatePos
  (a, syntax') <- fa syntax
  end <- gets pstatePos
  return ((a, SourceRegion start end), syntax')

coerceLexeme :: SyntaxKind -> String -> Action String
coerceLexeme _ description [] = failParse $ MissingSyntax description
coerceLexeme wantedKind description (SyntaxNode k w _ : ns)
  | k == wantedKind = do
    lexeme <- consume w
    return (lexeme, ns)
  | S.syntaxKindIsTrivia k = advance w >> coerceLexeme wantedKind description ns
  | otherwise = failParse $ UnexpectedNode k $ "instead of " ++ description

consume :: Int -> ParserM String
consume amount = do
  lexeme <- gets (take amount . pstateRemainingSource)
  advance amount
  return lexeme

advance :: Int -> ParserM ()
advance 0 = return ()
advance amount = do
  gets (head . pstateRemainingSource) >>= \case
    '\n' -> modify (\s -> s {pstateLine = pstateLine s + 1, pstateColumn = 1})
    _ -> modify (\s -> s {pstateColumn = pstateColumn s + 1})
  modify (\s -> s {pstateRemainingSource = tail (pstateRemainingSource s)})
  advance (amount - 1)

failParse :: ASTCoercionError -> ParserM a
failParse reason = do
  pos <- SourcePosition <$> gets pstateLine <*> gets pstateColumn
  throwError $ ASTCoercionError pos reason
