{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.Parser
  ( parseStatements,
  -- parseExpression
  )
where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Maybe (catMaybes, fromJust)
import Wasp.Analyzer.Parser (SourcePosition (SourcePosition), WithCtx (WithCtx), ctxFromRgn)
import Wasp.Analyzer.Parser.AST (AST, Expr, Stmt)
import Wasp.Analyzer.Parser.AST as AST
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind, SyntaxNode (SyntaxNode))
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as S
import Wasp.Analyzer.Parser.ParseError
import qualified Wasp.Analyzer.Parser.Token as T

data ParseState = ParseState
  { pstateLine :: !Int,
    pstateColumn :: !Int,
    pstateRemainingSource :: String
  }

pstatePos :: ParseState -> SourcePosition
pstatePos s = SourcePosition (pstateLine s) (pstateColumn s)

type ParserM a = StateT ParseState (Except ParseError) a

-- | @parseStatements sourceString syntax@ tries to convert a concrete syntax
-- tree into an AST.
parseStatements :: String -> [SyntaxNode] -> Either ParseError AST
parseStatements source syntax = runExcept $ evalStateT (coerceProgram syntax) initialState
  where
    initialState =
      ParseState
        { pstateLine = 0,
          pstateColumn = 0,
          pstateRemainingSource = source
        }

-- | Try to turn CST into top-level AST.
coerceProgram :: [SyntaxNode] -> ParserM AST
coerceProgram [] = return $ AST.AST []
coerceProgram (SyntaxNode S.Program _ children : _) = AST.AST . catMaybes <$> mapM coerceStmt children
coerceProgram (SyntaxNode k w _ : ns)
  | S.syntaxKindIsTrivia k = advance w >> coerceProgram ns
  | otherwise = failParse "Unexpected syntax at top-level"

-- | Try to turn CST into Stmt AST. Returns @Nothing@ if the given node is a
-- trivia node.
coerceStmt :: SyntaxNode -> ParserM (Maybe (WithCtx Stmt))
coerceStmt (SyntaxNode S.Decl _ children) = do
  startPos <- gets pstatePos
  (declType, children') <- coerceLexeme S.DeclType "declaration type" children
  (declName, children'') <- coerceLexeme S.DeclName "declaration name" children'
  (expr, children''') <- coerceExpr children''
  endPos <- gets pstatePos
  mapM_ (advance . S.snodeWidth) children'''
  return $ Just $ WithCtx (ctxFromRgn startPos endPos) (Decl declType declName expr)
coerceStmt (SyntaxNode k w _)
  | S.syntaxKindIsTrivia k = advance w >> return Nothing
  | otherwise = failParse "Unexpected syntax at top-level"

-- | Try to turn CST into Expr AST. Returns @(expr, remainingNodesFromInput)@
-- when successful.
coerceExpr :: [SyntaxNode] -> ParserM (WithCtx Expr, [SyntaxNode])
coerceExpr [] = failParse "Could not find expression"
coerceExpr (SyntaxNode k w children : ns)
  | S.syntaxKindIsTrivia k = advance w >> coerceExpr ns
  | otherwise = do
    startPos <- gets pstatePos
    expr <- case k of
      S.String -> AST.StringLiteral . tail . init <$> consume w
      S.Int -> AST.IntegerLiteral . fromJust . read <$> consume w
      S.Double -> AST.DoubleLiteral . fromJust . read <$> consume w
      S.BoolTrue -> advance w >> return (AST.BoolLiteral True)
      S.BoolFalse -> advance w >> return (AST.BoolLiteral False)
      S.Var -> AST.Var <$> consume w
      S.Dict -> coerceDict children
      S.List -> coerceList children
      S.Tuple -> coerceTuple children
      S.ExtImport -> coerceExtImport children
      S.Quoter -> coerceQuoter children
      _ -> failParse "Unexpected syntax where an expression was expected"
    endPos <- gets pstatePos
    return (WithCtx (ctxFromRgn startPos endPos) expr, ns)

coerceDict :: [SyntaxNode] -> ParserM Expr
coerceDict = undefined

coerceList :: [SyntaxNode] -> ParserM Expr
coerceList = undefined

coerceTuple :: [SyntaxNode] -> ParserM Expr
coerceTuple = undefined

coerceExtImport :: [SyntaxNode] -> ParserM Expr
coerceExtImport = undefined

coerceQuoter :: [SyntaxNode] -> ParserM Expr
coerceQuoter = undefined

coerceLexeme :: SyntaxKind -> String -> [SyntaxNode] -> ParserM (String, [SyntaxNode])
coerceLexeme _ description [] = failParse $ "Could not find " ++ description
coerceLexeme wantedKind description (SyntaxNode k w _ : ns)
  | k == wantedKind = do
    lexeme <- consume w
    return (lexeme, ns)
  | otherwise = advance w >> coerceLexeme wantedKind description ns

consume :: Int -> ParserM String
consume amount = do
  lexeme <- gets (take amount . pstateRemainingSource)
  advance amount
  return lexeme

advance :: Int -> ParserM ()
advance 0 = return ()
advance amount = do
  gets (head . pstateRemainingSource) >>= \case
    '\n' -> modify (\s -> s {pstateLine = pstateLine s + 1, pstateColumn = 0})
    _ -> modify (\s -> s {pstateColumn = pstateColumn s + 1})
  modify (\s -> s {pstateRemainingSource = tail (pstateRemainingSource s)})
  advance (amount - 1)

failParse :: String -> ParserM a
failParse reason = do
  pos <- SourcePosition <$> gets pstateLine <*> gets pstateColumn
  throwError $ ASTCoercionError pos reason
