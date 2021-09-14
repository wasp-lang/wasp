module Analyzer.Evaluator.EvaluationError
  ( EvaluationError (..),
    EvaluationErrorContext (..),
  )
where

import Analyzer.Type (Type)

data EvaluationError
  = -- | "ExpectedType expected actual"
    ExpectedType Type Type
  | -- | "ExpectedDictType actual"
    ExpectedDictType Type
  | -- | "ExpectedListType actual"
    ExpectedListType Type
  | -- | "UndefinedVariable varName"
    UndefinedVariable String
  | -- | "InvalidEnumVariant enumType enumValue"
    InvalidEnumVariant String String
  | -- | "MissingField fieldName"
    MissingField String
  | WithContext EvaluationErrorContext EvaluationError
  deriving (Show, Eq)

data EvaluationErrorContext
  = -- | InField fieldName
    InField String
  | InList
  | -- | ForVariable varName
    ForVariable String
  deriving (Show, Eq)
