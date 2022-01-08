module Wasp.Analyzer.Evaluator
  ( EvaluationError (..),
    evaluate,
    Decl,
    takeDecls,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)
import Wasp.Analyzer.Evaluator.Bindings (Bindings)
import Wasp.Analyzer.Evaluator.EvaluationError (EvaluationError (..))
import qualified Wasp.Analyzer.Type as Type
import qualified Wasp.Analyzer.TypeChecker.AST as AST
import qualified Wasp.Analyzer.TypeDefinitions as TD
import Wasp.AppSpec.Core.Decl (Decl, takeDecls)

-- | Evaluate type-checked AST to produce a list of declarations, which are the final output of Analyzer.
evaluate :: TD.TypeDefinitions -> AST.TypedAST -> Either EvaluationError [Decl]
evaluate typeDefs (AST.TypedAST stmts) = runExcept $ flip runReaderT typeDefs $ evalStateT (evalStmts stmts) H.empty

evalStmts :: [AST.WithCtx AST.TypedStmt] -> Eval [Decl]
evalStmts = traverse evalStmt

evalStmt :: AST.WithCtx AST.TypedStmt -> Eval Decl
evalStmt (AST.WithCtx _ctx (AST.Decl name param (Type.DeclType declTypeName))) = do
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
evalStmt (AST.WithCtx _ AST.Decl {}) = error "impossible: Decl statement has non-Decl type after type checking"

type Eval a = StateT Bindings (ReaderT TD.TypeDefinitions (Except EvaluationError)) a
