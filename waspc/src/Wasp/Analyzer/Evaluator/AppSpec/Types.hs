module Wasp.Analyzer.Evaluator.AppSpec.Types
  ( -- | While most of the types from AppSpec are supplied to the Analyzer / Evaluator through
    -- declarations (IsDeclType) and enums (IsEnumType) and therefore abstracted, there are couple
    -- of types/nodes from AppSpec that are not abstracted and are instead used directly by the Evaluator
    -- when evaluating type-checked AST into the AppSpec.
    -- This module re-exports those types and exists merely for convenience,
    -- to make it very obvious which types those are.
    ExtImport (..),
    JSON (..),
    PSL (..),
  )
where

import Wasp.Analyzer.TypeChecker.AST (ExtImportName)
import Wasp.AppSpec.Entity (PSL (..))

-- | TODO: Import from AppSpec instead of defining here.
data ExtImport = ExtImport ExtImportName String deriving (Eq, Show)

-- | TODO: Import from AppSpec instead of defining here.
newtype JSON = JSON String deriving (Eq, Show)
