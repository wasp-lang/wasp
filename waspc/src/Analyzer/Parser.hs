module Analyzer.Parser
  ( parse
  , AST (..)
  , Stmt (..)
  , Expr (..)
  , Ident
  , ExtImportName (..)
  , ParseError (..)
  , Posn (..)
  , Token (..)
  , TokenClass (..)
  ) where

import qualified Analyzer.Parser.Parser as P
import Analyzer.Parser.Util (initialState)
import Analyzer.Parser.Syntax
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Except (runExcept)

parse :: String -> Either ParseError AST
parse = runExcept . evalStateT P.parse . initialState
