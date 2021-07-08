module Analyzer.Parser
  ( -- * Overview

    -- | The "Analyzer.Parser" module is built of two parts:
    --
    -- - The lexer, generated with Alex, which creates tokens from a string of
    --   the wasp language
    -- - The parser, generated with Happy, which builds an abstract syntax
    --   tree from the tokens
    --
    -- These two parts don't really exist as separate passes over the source:
    -- they happen simultaneously. The parser asks the lexer to create tokens
    -- one at a time as needed.
    --
    -- A "Parser" monad ties both of the parts together; it manages state and
    -- exceptions for the parser and the lexer.
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
