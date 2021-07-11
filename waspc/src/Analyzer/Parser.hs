module Analyzer.Parser
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
    Ident,
    ExtImportName (..),
    ParseError (..),
    SourcePosition (..),
    Token (..),
    TokenClass (..),
  )
where

import Analyzer.Parser.AST
import Analyzer.Parser.ParseError
import qualified Analyzer.Parser.Parser as P
import Analyzer.Parser.Token
import Analyzer.Parser.Util (initialState)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.State (evalStateT)

parse :: String -> Either ParseError AST
parse = runExcept . evalStateT P.parse . initialState
