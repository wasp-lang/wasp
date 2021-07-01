module Analyzer.Parser
  ( parse,
    AST (..),
    Stmt (..),
    Expr (..),
    Ident,
    ExtImportName (..),
    ParseError (..),
    Posn (..),
    Token (..),
    TokenClass (..),
  )
where

import qualified Analyzer.Parser.Parser as P
import Analyzer.Parser.Syntax
import Analyzer.Parser.Util (initialState)
import Control.Monad.Trans.Except (runExcept)
import Control.Monad.Trans.State (evalStateT)

parse :: String -> Either ParseError AST
parse = runExcept . evalStateT P.parse . initialState
