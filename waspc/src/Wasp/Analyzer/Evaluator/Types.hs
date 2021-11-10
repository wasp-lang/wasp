module Wasp.Analyzer.Evaluator.Types
  ( ExtImport (..),
    JSON (..),
    PSL (..),
  )
where

-- TODO:
-- After refactoring "Analyzer.StdTypeDefinitions" to use types in "Wasp"
-- module, this module can be deleted and the correct types from there will
-- be used in the result of evaluation.

import Wasp.Analyzer.TypeChecker.AST (ExtImportName)

data ExtImport = ExtImport ExtImportName String deriving (Eq, Show)

newtype JSON = JSON String deriving (Eq, Show)

newtype PSL = PSL String deriving (Eq, Show)
