{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.Valid.Common
  ( validateFieldValue,
    validateRequiredField,
    validateArrayFieldIncludesRequired,
    FullyQualifiedFieldName (FieldName),
  )
where

import Data.List (intercalate)
import qualified Data.Set as S
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))

class JsonValue a where
  showAsJsValue :: a -> String

instance JsonValue String where
  showAsJsValue = show

instance JsonValue [String] where
  showAsJsValue = show

instance JsonValue Bool where
  showAsJsValue True = "true"
  showAsJsValue False = "false"

-- | Represents a fully qualified field name in a JSON object.
-- For example, for the field "module" in the "compilerOptions" object,
-- the fully qualified field name would be "compilerOptions.module".
data FullyQualifiedFieldName = FieldName FieldPath

type FieldPath = [String]

instance Show FullyQualifiedFieldName where
  show (FieldName fieldPath) = intercalate "." fieldPath

validateRequiredField :: (Eq a, JsonValue a) => String -> FullyQualifiedFieldName -> Maybe a -> a -> [GeneratorError]
validateRequiredField fileName fullyQualifiedFieldName fieldValue expectedValue =
  validateFieldValue fileName fullyQualifiedFieldName (Just expectedValue) fieldValue

validateFieldValue :: (Eq a, JsonValue a) => String -> FullyQualifiedFieldName -> Maybe a -> Maybe a -> [GeneratorError]
validateFieldValue fileName fullyQualifiedFieldName expectedValue actualValue =
  case (expectedValue, actualValue) of
    (Nothing, Nothing) -> []
    (Just expected, Just actual) -> [makeInvalidValueErrorMessage expected | actual /= expected]
    (Just expected, Nothing) -> [makeMissingFieldErrorMessage fileName fullyQualifiedFieldName expected]
    (Nothing, Just _) -> [fieldMustBeUnsetErrorMessage]
  where
    makeInvalidValueErrorMessage expected =
      GenericGeneratorError $
        unwords
          [ "Invalid value for the",
            "\"" ++ show fullyQualifiedFieldName ++ "\"",
            "field in",
            fileName ++ ",",
            "you must set it to:",
            showAsJsValue expected ++ "."
          ]

    fieldMustBeUnsetErrorMessage =
      GenericGeneratorError $
        unwords
          [ "The",
            "\"" ++ show fullyQualifiedFieldName ++ "\"",
            "field in",
            fileName,
            "must be left unspecified."
          ]

validateArrayFieldIncludesRequired :: (Ord a, JsonValue [a]) => String -> FullyQualifiedFieldName -> [a] -> Maybe [a] -> [GeneratorError]
validateArrayFieldIncludesRequired fileName fullyQualifiedFieldName requiredValues Nothing =
  [makeMissingFieldErrorMessage fileName fullyQualifiedFieldName requiredValues]
validateArrayFieldIncludesRequired fileName fullyQualifiedFieldName requiredValues (Just actualValue) =
  if null missingSet
    then []
    else
      [ GenericGeneratorError $
          unwords
            [ "The",
              "\"" ++ show fullyQualifiedFieldName ++ "\"",
              "field in",
              fileName,
              "is missing required values:",
              showAsJsValue (S.toList missingSet) ++ ".",
              "Current values:",
              showAsJsValue actualValue ++ "."
            ]
      ]
  where
    missingSet = requiredSet `S.difference` actualSet

    requiredSet = S.fromList requiredValues
    actualSet = S.fromList actualValue

makeMissingFieldErrorMessage :: JsonValue a => String -> FullyQualifiedFieldName -> a -> GeneratorError
makeMissingFieldErrorMessage fileName fullyQualifiedFieldName expected =
  GenericGeneratorError $
    unwords
      [ "The",
        "\"" ++ show fullyQualifiedFieldName ++ "\"",
        "field is missing in",
        fileName ++ ",",
        "you must set it to:",
        showAsJsValue expected ++ "."
      ]
