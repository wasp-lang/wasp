-- | This module exports the functions necessary for typechecking the AST from "Analyzer.Parser".
-- See the Type Checking section in "docs/wasplang" for the rules type checking follows.
--
-- See "Analyzer.TypeChecker.Internal" for the implementation of the type checking rules.
module Wasp.Analyzer.TypeChecker
  ( -- * Types

    -- ** AST
    TypedAST (..),
    TypedStmt (..),
    TypedExpr (..),

    -- ** Errors
    TypeError (..),
    TypeCoerceReason (..),

    -- * Type Checking Functions
    typeCheck,
    exprType,
  )
where

import Wasp.Analyzer.Parser.AST (AST)
import Wasp.Analyzer.TypeChecker.AST
import Wasp.Analyzer.TypeChecker.Internal (check)
import Wasp.Analyzer.TypeChecker.Monad (run)
import Wasp.Analyzer.TypeChecker.TypeError
import Wasp.Analyzer.TypeDefinitions (TypeDefinitions)

-- | Checks that an AST conforms to the type rules of Wasp and produces
--   an AST labelled with type information.
typeCheck :: TypeDefinitions -> AST -> Either TypeError TypedAST
typeCheck typeDefs ast = run typeDefs $ check ast
