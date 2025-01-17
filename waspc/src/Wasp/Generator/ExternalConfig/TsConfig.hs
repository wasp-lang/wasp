{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.ExternalConfig.TsConfig
  ( validateSrcTsConfig,
  )
where

import Data.List (intercalate)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.ExternalConfig.Common (ErrorMsg)

class IsJavascriptValue a where
  showAsJsValue :: a -> String

instance IsJavascriptValue String where
  showAsJsValue = show

instance IsJavascriptValue [String] where
  showAsJsValue = show

instance IsJavascriptValue Bool where
  showAsJsValue True = "true"
  showAsJsValue False = "false"

-- | Represents a fully qualified field name in a JSON object.
-- For example, for the field "module" in the "compilerOptions" object,
-- the fully qualified field name would be "compilerOptions.module".
data FullyQualifiedFieldName = FieldName FieldPath

type FieldPath = [String]

instance Show FullyQualifiedFieldName where
  show (FieldName fieldPath) = intercalate "." fieldPath

validateSrcTsConfig :: T.TsConfig -> [ErrorMsg]
validateSrcTsConfig tsConfig =
  validateCompilerOptions (T.compilerOptions tsConfig)

validateCompilerOptions :: T.CompilerOptions -> [ErrorMsg]
validateCompilerOptions compilerOptions =
  concat
    [ validateRequiredFieldInCompilerOptions "module" T._module "esnext",
      validateRequiredFieldInCompilerOptions "target" T.target "esnext",
      validateRequiredFieldInCompilerOptions "moduleResolution" T.moduleResolution "bundler",
      validateRequiredFieldInCompilerOptions "jsx" T.jsx "preserve",
      validateRequiredFieldInCompilerOptions "strict" T.strict True,
      validateRequiredFieldInCompilerOptions "esModuleInterop" T.esModuleInterop True,
      validateRequiredFieldInCompilerOptions "lib" T.lib ["dom", "dom.iterable", "esnext"],
      validateRequiredFieldInCompilerOptions "allowJs" T.allowJs True,
      validateRequiredFieldInCompilerOptions "typeRoots" T.typeRoots ["node_modules/@testing-library", "node_modules/@types"],
      validateRequiredFieldInCompilerOptions "outDir" T.outDir ".wasp/out/user",
      validateRequiredFieldInCompilerOptions "composite" T.composite True,
      validateFieldIsUnset (FieldName ["compilerOptions", "baseUrl"]) (T.baseUrl compilerOptions)
    ]
  where
    validateRequiredFieldInCompilerOptions relativeFieldName getFieldValue =
      validateRequiredField (FieldName ["compilerOptions", relativeFieldName]) (getFieldValue compilerOptions)

validateRequiredField :: (Eq a, IsJavascriptValue a) => FullyQualifiedFieldName -> Maybe a -> a -> [String]
validateRequiredField fullyQualifiedFieldName fieldValue expectedValue =
  validateFieldValue fullyQualifiedFieldName (Just expectedValue) fieldValue

validateFieldIsUnset :: (Eq a, IsJavascriptValue a) => FullyQualifiedFieldName -> Maybe a -> [String]
validateFieldIsUnset fullyQualifiedFieldName fieldValue =
  validateFieldValue fullyQualifiedFieldName Nothing fieldValue

validateFieldValue :: (Eq a, IsJavascriptValue a) => FullyQualifiedFieldName -> Maybe a -> Maybe a -> [String]
validateFieldValue fullyQualifiedFieldName expectedValue actualValue =
  case (expectedValue, actualValue) of
    (Just expected, Just actual) -> [makeInvalidValueErrorMessage expected | actual /= expected]
    (Nothing, Nothing) -> []
    (Just expected, Nothing) -> [makeMissingFieldErrorMessage expected]
    (Nothing, Just _) -> [setFieldErrorMessage]
  where
    makeInvalidValueErrorMessage expected =
      unwords
        [ "Invalid value for the",
          "\"" ++ show fullyQualifiedFieldName ++ "\"",
          "field in tsconfig.json, expected value:",
          showAsJsValue expected ++ "."
        ]

    setFieldErrorMessage =
      unwords
        [ "The",
          "\"" ++ show fullyQualifiedFieldName ++ "\"",
          "should be unset"
        ]

    makeMissingFieldErrorMessage expected =
      unwords
        [ "The",
          "\"" ++ show fullyQualifiedFieldName ++ "\"",
          "field is missing in tsconfig.json, expected value:",
          showAsJsValue expected ++ "."
        ]
