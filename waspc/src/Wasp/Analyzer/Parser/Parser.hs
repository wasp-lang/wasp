{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.Parser
  ( parseStatements,
  -- parseExpression
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Data.Maybe (catMaybes)
import Wasp.Analyzer.Parser.AST (AST, Expr, Stmt)
import Wasp.Analyzer.Parser.AST as AST
import Wasp.Analyzer.Parser.ConcreteParser.CST (SyntaxKind, SyntaxNode (SyntaxNode))
import qualified Wasp.Analyzer.Parser.ConcreteParser.CST as S
import Wasp.Analyzer.Parser.Ctx (WithCtx (WithCtx), ctxFromRgn)
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))
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
      _ -> failParse "Unexpected syntax where an expression was expected"
    endPos <- gets pstatePos
    return (WithCtx (ctxFromRgn startPos endPos) expr, ns)

coerceDict :: [SyntaxNode] -> ParserM Expr
coerceDict syntax = do
  (_, syntax') <- coerceLexeme (S.Token T.LCurly) "{" syntax
  (entries, syntax'') <- coerceEntries syntax'
  (_, syntax''') <- coerceLexeme (S.Token T.RCurly) "}" syntax''
  mapM_ (advance . S.snodeWidth) syntax'''
  return $ Dict entries

coerceEntries :: [SyntaxNode] -> ParserM ([(Identifier, WithCtx Expr)], [SyntaxNode])
coerceEntries [] = return ([], [])
coerceEntries (n@(SyntaxNode k w children) : ns)
  | k == S.DictEntry = (first . (:)) <$> coerceEntry children <*> coerceEntries ns
  | k == S.Token T.Comma || S.syntaxKindIsTrivia k = advance w >> coerceEntries ns
  | otherwise = return ([], n : ns)

coerceEntry :: [SyntaxNode] -> ParserM (Identifier, WithCtx Expr)
coerceEntry syntax = do
  (dictKey, syntax') <- coerceLexeme S.DictKey "dictionary key" syntax
  (_, syntax'') <- coerceLexeme (S.Token T.Colon) ":" syntax'
  (expr, syntax''') <- coerceExpr syntax''
  mapM_ (advance . S.snodeWidth) syntax'''
  return (dictKey, expr)

coerceList :: [SyntaxNode] -> ParserM Expr
coerceList = undefined

coerceTuple :: [SyntaxNode] -> ParserM Expr
coerceTuple = undefined

coerceExtImport :: [SyntaxNode] -> ParserM Expr
coerceExtImport = undefined

coerceQuoter :: [SyntaxNode] -> ParserM Expr
coerceQuoter syntax = do
  lquoteStart <- gets pstatePos
  (lquote, syntax') <- coerceLexeme (S.Token T.LQuote) "{=tag" syntax
  lquoteEnd <- gets pstatePos
  (contents, syntax'') <- first concat <$> collectQuoted syntax'
  rquoteStart <- gets pstatePos
  (rquote, syntax''') <- coerceLexeme (S.Token T.RQuote) "tag=}" syntax''
  rquoteEnd <- gets pstatePos
  mapM_ (advance . S.snodeWidth) syntax'''
  let ltag = drop 2 lquote
  let rtag = take (length rquote - 2) rquote
  if ltag /= rtag
    then
      throwError $
        QuoterDifferentTags
          (WithCtx (ctxFromRgn lquoteStart lquoteEnd) lquote)
          (WithCtx (ctxFromRgn rquoteStart rquoteEnd) rquote)
    else return $ Quoter ltag contents

collectQuoted :: [SyntaxNode] -> ParserM ([String], [SyntaxNode])
collectQuoted [] = return ([], [])
collectQuoted (n@(SyntaxNode k w _) : ns)
  | k == S.Token T.Quoted = do
    lexeme <- consume w
    (lexemes, remaining) <- collectQuoted ns
    return (lexeme : lexemes, remaining)
  | k == S.Token T.RQuote = return ([], n : ns)
  | otherwise = failParse "Unexpected syntax inside quoter (this is a bug in waspc)"

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
