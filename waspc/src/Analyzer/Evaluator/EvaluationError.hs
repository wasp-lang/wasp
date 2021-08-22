module Analyzer.Evaluator.EvaluationError
  ( EvaluationError (..),
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
    InValidEnumVariant String String
  | -- | "MissingField fieldName"
    MissingField String
  | -- | An evaluation error for a named variable
    ForVariable String EvaluationError
  | -- | "InField fieldName innerError"
    InField String EvaluationError
  | -- | "InList innerError"
    InList EvaluationError
  deriving (Show, Eq)
