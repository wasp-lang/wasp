module Analyzer.Evaluator.Types
  ( ExtImport (..),
    JSON (..),
    PSL (..),
  )
where

import Analyzer.TypeChecker.AST (ExtImportName)

data ExtImport = ExtImport ExtImportName String deriving (Eq, Show)

newtype JSON = JSON String deriving (Eq, Show)

newtype PSL = PSL String deriving (Eq, Show)
