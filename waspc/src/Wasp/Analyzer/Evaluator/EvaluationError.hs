{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Evaluator.EvaluationError
  ( EvaluationError (..),
    EvalErrorCtx (..),
    EvaluationParseError (..),
    EvaluationError' (..),
    mkEvaluationError,
    getErrorMessageAndCtx,
  )
where

import Control.Arrow (first)
import Data.List (intercalate)
import qualified Text.Parsec
import Wasp.Analyzer.Parser.Ctx (Ctx, WithCtx (..))
import Wasp.Analyzer.Type (Type)
import Wasp.Util (concatPrefixAndText, indent)

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
getErrorMessageAndCtx (EvaluationError (WithCtx ctx evalError)) = case evalError of
  ExpectedType expectedType actualType ->
    ( intercalate
        "\n"
        [ concatPrefixAndText "Expected type: " (show expectedType),
          concatPrefixAndText "Actual type:   " (show actualType)
        ],
      ctx
    )
  ExpectedDictType actualType ->
    ( intercalate
        "\n"
        [ "Expected a dictionary.",
          concatPrefixAndText "Actual type: " (show actualType)
        ],
      ctx
    )
  ExpectedListType actualType ->
    ( intercalate
        "\n"
        [ "Expected a list.",
          concatPrefixAndText "Actual type: " (show actualType)
        ],
      ctx
    )
  ExpectedTupleType expectedTupleSize actualType ->
    ( intercalate
        "\n"
        [ "Expected a tuple of size " ++ show expectedTupleSize ++ ".",
          concatPrefixAndText "Actual type: " (show actualType)
        ],
      ctx
    )
  UndefinedVariable varName -> ("Undefined variable " ++ varName, ctx)
  InvalidEnumVariant enumType validEnumVariants actualEnumVariant ->
    ( "Expected value of enum type '" ++ enumType
        ++ "' but got value '"
        ++ actualEnumVariant
        ++ "'\n"
        ++ "Valid values: "
        ++ intercalate " | " validEnumVariants,
      ctx
    )
  MissingDictField fieldName -> ("Missing dictionary field '" ++ fieldName ++ "'", ctx)
  ParseError (EvaluationParseErrorParsec e) -> ("Parse error:\n" ++ indent 2 (show e), ctx)
  ParseError (EvaluationParseError msg) -> ("Parse error:\n" ++ indent 2 msg, ctx)
  WithEvalErrorCtx evalCtx subError ->
    let evalCtxMsg = case evalCtx of
          InField fieldName -> "In dictionary field '" ++ fieldName ++ "':"
          InList -> "In list:"
          InTuple -> "In tuple:"
          ForVariable varName -> "For variable '" ++ varName ++ "':"
     in first (((evalCtxMsg ++ "\n") ++) . indent 2) $ getErrorMessageAndCtx subError
