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
    parse,
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
    TokenType (..),
  )
where

import Control.Monad.Except (runExcept)
import Control.Monad.State (evalStateT)
import Wasp.Analyzer.Parser.AST
import Wasp.Analyzer.Parser.Ctx (Ctx (..), WithCtx (..), ctxFromPos, ctxFromRgn, fromWithCtx, getCtxRgn, withCtx)
import Wasp.Analyzer.Parser.Monad (makeInitialState)
import Wasp.Analyzer.Parser.ParseError
import qualified Wasp.Analyzer.Parser.Parser as P
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..))
import Wasp.Analyzer.Parser.Token

parse :: String -> Either ParseError AST
parse = runExcept . evalStateT P.parse . makeInitialState

parseExpression :: String -> Either ParseError Expr
parseExpression = runExcept . evalStateT  P.parseExpression . makeInitialState 

isValidWaspIdentifier :: String -> Bool
isValidWaspIdentifier str = case parseExpression str of
  Right (Var name) -> let noCharsSkipped = length name == length str in noCharsSkipped
  _ -> False