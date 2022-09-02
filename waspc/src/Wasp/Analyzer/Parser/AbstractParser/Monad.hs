{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.AbstractParser.Monad
  ( ParseState (..),
    pstatePos,
    ParserM,
    runParserM,
    consume,
    advance,
    unexpectedNode,
    throwMissingSyntax,
  )
where

import Control.Monad.Except (Except, runExcept, throwError)
import Control.Monad.State.Strict (StateT, evalStateT, gets, modify)
import Wasp.Analyzer.Parser.CST (SyntaxKind)
import Wasp.Analyzer.Parser.ParseError
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (SourcePosition))

data ParseState = ParseState
  { pstateLine :: !Int,
    pstateColumn :: !Int,
    pstateLastLineLength :: !Int,
    pstateRemainingSource :: String
  }

pstatePos :: ParseState -> SourcePosition
pstatePos s = SourcePosition (pstateLine s) (pstateColumn s)

type ParserM a = StateT ParseState (Except ParseError) a

runParserM :: String -> ParserM a -> Either ParseError a
runParserM source parser = runExcept $ evalStateT parser initialState
  where
    initialState =
      ParseState
        { pstateLine = 1,
          pstateColumn = 1,
          pstateLastLineLength = 1,
          pstateRemainingSource = source
        }

consume :: Int -> ParserM String
consume amount = do
  lexeme <- gets (take amount . pstateRemainingSource)
  advance amount
  return lexeme

advance :: Int -> ParserM ()
advance 0 = return ()
advance amount = do
  gets (head . pstateRemainingSource) >>= \case
    '\n' -> modify (\s -> s {pstateLine = pstateLine s + 1, pstateColumn = 1, pstateLastLineLength = pstateColumn s})
    _ -> modify (\s -> s {pstateColumn = pstateColumn s + 1})
  modify (\s -> s {pstateRemainingSource = tail (pstateRemainingSource s)})
  advance (amount - 1)

-- | Returns a GHC error. Use this when a node is found in the CST that should
-- not be in that position. This scenario is a bug in the parser, which is why
-- it crashes waspc.
unexpectedNode :: SyntaxKind -> String -> ParserM a
unexpectedNode unexpectedKind locationDescription =
  error $ "Unexpected syntax " ++ show unexpectedKind ++ " " ++ locationDescription ++ " created by CST"

throwMissingSyntax :: String -> ParserM a
throwMissingSyntax reason = do
  pos <- SourcePosition <$> gets pstateLine <*> gets pstateColumn
  throwError $ MissingSyntax pos reason
