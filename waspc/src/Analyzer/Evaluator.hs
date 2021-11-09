module Analyzer.Evaluator
  ( EvaluationError (..),
    evaluate,
    Decl,
    takeDecls,
    module Analyzer.Evaluator.Types,
  )
where

import Analyzer.Evaluator.Bindings (Bindings)
import Analyzer.Evaluator.EvaluationError (EvaluationError (..))
import Analyzer.Evaluator.Types
import qualified Analyzer.Type as Type
import qualified Analyzer.TypeChecker.AST as AST
import qualified Analyzer.TypeDefinitions as TD
import AppSpec.AST.Core.Decl (Decl, takeDecls)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)

-- | Evaluate type-checked AST to produce a list of declarations, which are the final output of Analyzer.
evaluate :: TD.TypeDefinitions -> AST.TypedAST -> Either EvaluationError [Decl]
evaluate typeDefs (AST.TypedAST stmts) = runExcept $ flip runReaderT typeDefs $ evalStateT (evalStmts stmts) H.empty

evalStmts :: [AST.TypedStmt] -> Eval [Decl]
evalStmts = traverse evalStmt

evalStmt :: AST.TypedStmt -> Eval Decl
evalStmt (AST.Decl name param (Type.DeclType declTypeName)) = do
  declType <-
    asks
      ( fromMaybe
          (error "impossible: Decl statement has non-existant type after type checking")
          . TD.getDeclType declTypeName
      )
  typeDefs <- ask
  bindings <- get
  case TD.dtEvaluate declType typeDefs bindings name param of
    Left err -> throwError err
    Right decl -> modify (H.insert name decl) >> return decl
evalStmt AST.Decl {} = error "impossible: Decl statement has non-Decl type after type checking"

type Eval a = StateT Bindings (ReaderT TD.TypeDefinitions (Except EvaluationError)) a
