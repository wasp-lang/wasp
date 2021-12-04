module Wasp.Analyzer.Evaluator.EvaluationError
  ( EvaluationError (..),
    EvaluationErrorContext (..),
    EvaluationParseError (..),
  )
where

import qualified Text.Parsec
import Wasp.Analyzer.Type (Type)

data EvaluationError
  = -- | "ExpectedType expected actual"
    ExpectedType Type Type
  | -- | "ExpectedDictType actual"
    ExpectedDictType Type
  | -- | "ExpectedListType actual"
    ExpectedListType Type
  | -- | "ExpectedTupleType expectedTupleSize actualType"
    ExpectedTupleType Int Type
  | -- | "UndefinedVariable varName"
    UndefinedVariable String
  | -- | "InvalidEnumVariant enumType enumValue"
    InvalidEnumVariant String String
  | -- | "MissingField fieldName"
    MissingField String
  | -- | In case when evaluation includes parsing and it fails.
    ParseError EvaluationParseError
  | -- | Not an actual error, but a wrapper that provides additional context.
    WithContext EvaluationErrorContext EvaluationError
  deriving (Show, Eq)

data EvaluationErrorContext
  = -- | InField fieldName
    InField String
  | InList
  | InTuple
  | -- | ForVariable varName
    ForVariable String
  deriving (Show, Eq)

data EvaluationParseError
  = -- | In case when evaluation includes parsing with Parsec and it fails.
    EvaluationParseErrorParsec Text.Parsec.ParseError
  | -- | In case when evaluation does some general parsing and it fails.
    EvaluationParseError String
  deriving (Show, Eq)
