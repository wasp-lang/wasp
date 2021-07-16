-- | This module exports the functions necessary typecheck AST from "Analyzer.Parser".
-- See the Type Checking section in "docs/wasplang" for the rules type checking follows.
--
-- A use of this module might look like:
--
-- > import qualified Analyzer.Parser
-- > import Analyzer.TypeChecker
-- > import Analyzer.StdTypeDefinitions
--
-- > ast :: Analyzer.Parser.AST
-- >
-- > typedAst :: TypedAST
-- > typedAst = case typeCheck stdTypes ast of
-- >           Left err -> error $ show err
-- >           Right typed -> typed
--
-- For detailed explanation of how type checking is done, see "Analyzer.TypeChecker.Internal"
module Analyzer.TypeChecker
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

import Analyzer.Parser.AST (AST)
import Analyzer.TypeChecker.AST
import Analyzer.TypeChecker.Internal (check)
import Analyzer.TypeChecker.Monad (runT)
import Analyzer.TypeChecker.TypeError
import Analyzer.TypeDefinitions (TypeDefinitions)

-- | Checks that an AST conforms to the type rules of Wasp and produces a
--   an AST labelled with type information.
typeCheck :: TypeDefinitions -> AST -> Either TypeError TypedAST
typeCheck tds ast = runT tds $ check ast
