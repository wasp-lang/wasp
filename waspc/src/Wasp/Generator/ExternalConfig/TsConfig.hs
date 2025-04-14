{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.ExternalConfig.TsConfig
  ( validateSrcTsConfig,
  )
where

import Data.List (intercalate)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.ExternalConfig.Common (ErrorMsg)

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

validateSrcTsConfig :: T.TsConfig -> [ErrorMsg]
validateSrcTsConfig tsConfig =
  validateRequiredField (FieldName ["include"]) (T.include tsConfig) ["src"]
    ++ validateCompilerOptions (T.compilerOptions tsConfig)

validateCompilerOptions :: T.CompilerOptions -> [ErrorMsg]
validateCompilerOptions compilerOptions =
  concat
    [ validateRequiredFieldInCompilerOptions "module" T._module "preserve",
      validateRequiredFieldInCompilerOptions "target" T.target "es2022",
      validateRequiredFieldInCompilerOptions "moduleResolution" T.moduleResolution "bundler",
      validateRequiredFieldInCompilerOptions "moduleDetection" T.moduleDetection "force",
      validateRequiredFieldInCompilerOptions "isolatedModules" T.isolatedModules True,
      validateRequiredFieldInCompilerOptions "jsx" T.jsx "preserve",
      validateRequiredFieldInCompilerOptions "strict" T.strict True,
      validateRequiredFieldInCompilerOptions "esModuleInterop" T.esModuleInterop True,
      validateRequiredFieldInCompilerOptions "lib" T.lib ["dom", "dom.iterable", "esnext"],
      validateRequiredFieldInCompilerOptions "allowJs" T.allowJs True,
      validateRequiredFieldInCompilerOptions "typeRoots" T.typeRoots ["node_modules/@testing-library", "node_modules/@types"],
      validateRequiredFieldInCompilerOptions "outDir" T.outDir ".wasp/out/user",
      validateRequiredFieldInCompilerOptions "composite" T.composite True,
      validateRequiredFieldInCompilerOptions "skipLibCheck" T.skipLibCheck True
    ]
  where
    validateRequiredFieldInCompilerOptions relativeFieldName getFieldValue =
      validateRequiredField (FieldName ["compilerOptions", relativeFieldName]) (getFieldValue compilerOptions)

validateRequiredField :: (Eq a, JsonValue a) => FullyQualifiedFieldName -> Maybe a -> a -> [String]
validateRequiredField fullyQualifiedFieldName fieldValue expectedValue =
  validateFieldValue fullyQualifiedFieldName (Just expectedValue) fieldValue

validateFieldValue :: (Eq a, JsonValue a) => FullyQualifiedFieldName -> Maybe a -> Maybe a -> [String]
validateFieldValue fullyQualifiedFieldName expectedValue actualValue =
  case (expectedValue, actualValue) of
    (Nothing, Nothing) -> []
    (Just expected, Just actual) -> [makeInvalidValueErrorMessage expected | actual /= expected]
    (Just expected, Nothing) -> [makeMissingFieldErrorMessage expected]
    (Nothing, Just _) -> [fieldMustBeUnsetErrorMessage]
  where
    makeInvalidValueErrorMessage expected =
      unwords
        [ "Invalid value for the",
          "\"" ++ show fullyQualifiedFieldName ++ "\"",
          "field in TS config, you must set it to:",
          showAsJsValue expected ++ "."
        ]

    fieldMustBeUnsetErrorMessage =
      unwords
        [ "The",
          "\"" ++ show fullyQualifiedFieldName ++ "\"",
          "field in TS Config must be left unspecified."
        ]

    makeMissingFieldErrorMessage expected =
      unwords
        [ "The",
          "\"" ++ show fullyQualifiedFieldName ++ "\"",
          "field is missing in TS config, you must set it to:",
          showAsJsValue expected ++ "."
        ]
