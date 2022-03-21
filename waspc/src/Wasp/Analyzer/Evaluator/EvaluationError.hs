module Wasp.Analyzer.Evaluator.EvaluationError
  ( EvaluationError (..),
    EvalErrorCtx (..),
    EvaluationParseError (..),
    EvaluationError' (..),
    mkEvaluationError,
    getErrorMessageAndCtx
  )
where

import Data.List (intercalate)
import qualified Text.Parsec
import Wasp.Analyzer.Parser.Ctx (Ctx, WithCtx (..))
import Wasp.Analyzer.Type (Type)
import Wasp.Util (concatPrefixAndText, indent, second3)
import Wasp.Analyzer.ErrorMessage (makeFullErrorMsg)

newtype EvaluationError = EvaluationError (WithCtx EvaluationError')
  deriving (Show, Eq)

{- ORMOLU_DISABLE -}
data EvaluationError'
  = -- | "ExpectedType expected actual"
    ExpectedType       Type Type
  | -- | "ExpectedDictType actual"
    ExpectedDictType   Type
  | -- | "ExpectedListType actual"
    ExpectedListType   Type
  | -- | "ExpectedTupleType expectedTupleSize actualType"
    ExpectedTupleType Int Type
  | -- | "UndefinedVariable varName"
    UndefinedVariable  String
  | -- | "InvalidEnumVariant enumType validEnumVariants actualEnumVariant"
    InvalidEnumVariant String [String] String
  | -- | "MissingDictField fieldName"
    MissingDictField   String
  | -- | In case when evaluation includes parsing and it fails.
    ParseError         EvaluationParseError
  | -- | Not an actual error, but a wrapper that provides additional context.
    WithEvalErrorCtx   EvalErrorCtx EvaluationError
  deriving (Show, Eq)
{- ORMOLU_ENABLE -}

data EvalErrorCtx
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

mkEvaluationError :: Ctx -> EvaluationError' -> EvaluationError
mkEvaluationError ctx e = EvaluationError $ WithCtx ctx e

getErrorMessageAndCtx :: EvaluationError -> (String, Ctx)
getErrorMessageAndCtx err = (makeFullErrorMsg errorMsg errorCtxMsgs, ctx)
  where (errorMsg, errorCtxMsgs, ctx) = getErrorMsgAndErrorCtxMsgsAndParsingCtx err

getErrorMsgAndErrorCtxMsgsAndParsingCtx :: EvaluationError -> (String, [String], Ctx)
getErrorMsgAndErrorCtxMsgsAndParsingCtx (EvaluationError (WithCtx ctx evalError)) = case evalError of
  ExpectedType expectedType actualType ->
    makeMainMsg $
      intercalate
        "\n"
        [ concatPrefixAndText "Expected type: " (show expectedType),
          concatPrefixAndText "Actual type:   " (show actualType)
        ]
  ExpectedDictType actualType ->
    makeMainMsg $
      intercalate
        "\n"
        [ "Expected a dictionary.",
          concatPrefixAndText "Actual type: " (show actualType)
        ]
  ExpectedListType actualType ->
    makeMainMsg $
      intercalate
        "\n"
        [ "Expected a list.",
          concatPrefixAndText "Actual type: " (show actualType)
        ]
  ExpectedTupleType expectedTupleSize actualType ->
    makeMainMsg $
      intercalate
        "\n"
        [ "Expected a tuple of size " ++ show expectedTupleSize ++ ".",
          concatPrefixAndText "Actual type: " (show actualType)
        ]
  UndefinedVariable varName -> makeMainMsg $ "Undefined variable " ++ varName
  InvalidEnumVariant enumType validEnumVariants actualEnumVariant ->
    makeMainMsg $
      "Expected value of enum type '" ++ enumType
        ++ "' but got value '"
        ++ actualEnumVariant
        ++ "'\n"
        ++ "Valid values: "
        ++ intercalate " | " validEnumVariants
  MissingDictField fieldName -> makeMainMsg $ "Missing dictionary field '" ++ fieldName ++ "'"
  ParseError (EvaluationParseErrorParsec e) -> makeMainMsg ("Parse error:\n" ++ indent 2 (show e))
  ParseError (EvaluationParseError msg) -> makeMainMsg ("Parse error:\n" ++ indent 2 msg)
  WithEvalErrorCtx evalCtx subError -> second3 (evalCtxMsg evalCtx :) $ getErrorMsgAndErrorCtxMsgsAndParsingCtx subError
  where
    makeMainMsg msg = (msg, [], ctx)

evalCtxMsg :: EvalErrorCtx -> String
evalCtxMsg evalCtx = case evalCtx of
  (InField fieldName) -> "For dictionary field '" ++ fieldName ++ "'"
  InList -> "In list"
  InTuple -> "In tuple"
  (ForVariable varName) -> "For variable '" ++ varName
