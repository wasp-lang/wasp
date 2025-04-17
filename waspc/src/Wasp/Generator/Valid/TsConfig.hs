{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.Valid.TsConfig
  ( validateSrcTsConfig,
  )
where

import Data.List (intercalate)
import qualified Wasp.ExternalConfig.TsConfig as T
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

validateSrcTsConfig :: T.TsConfig -> [GeneratorError]
validateSrcTsConfig srcTsConfig =
  validateRequiredField (FieldName ["include"]) (T.include srcTsConfig) ["src"]
    ++ validateCompilerOptions (T.compilerOptions srcTsConfig)

validateCompilerOptions :: T.CompilerOptions -> [GeneratorError]
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
      validateRequiredFieldInCompilerOptions "skipLibCheck" T.skipLibCheck True
    ]
  where
    validateRequiredFieldInCompilerOptions relativeFieldName getFieldValue =
      validateRequiredField (FieldName ["compilerOptions", relativeFieldName]) (getFieldValue compilerOptions)

validateRequiredField :: (Eq a, JsonValue a) => FullyQualifiedFieldName -> Maybe a -> a -> [GeneratorError]
validateRequiredField fullyQualifiedFieldName fieldValue expectedValue =
  validateFieldValue fullyQualifiedFieldName (Just expectedValue) fieldValue

validateFieldValue :: (Eq a, JsonValue a) => FullyQualifiedFieldName -> Maybe a -> Maybe a -> [GeneratorError]
validateFieldValue fullyQualifiedFieldName expectedValue actualValue =
  case (expectedValue, actualValue) of
    (Nothing, Nothing) -> []
    (Just expected, Just actual) -> [makeInvalidValueErrorMessage expected | actual /= expected]
    (Just expected, Nothing) -> [makeMissingFieldErrorMessage expected]
    (Nothing, Just _) -> [fieldMustBeUnsetErrorMessage]
  where
    makeInvalidValueErrorMessage expected =
      GenericGeneratorError $
        unwords
          [ "Invalid value for the",
            "\"" ++ show fullyQualifiedFieldName ++ "\"",
            "field in TS config, you must set it to:",
            showAsJsValue expected ++ "."
          ]

    fieldMustBeUnsetErrorMessage =
      GenericGeneratorError $
        unwords
          [ "The",
            "\"" ++ show fullyQualifiedFieldName ++ "\"",
            "field in TS Config must be left unspecified."
          ]

    makeMissingFieldErrorMessage expected =
      GenericGeneratorError $
        unwords
          [ "The",
            "\"" ++ show fullyQualifiedFieldName ++ "\"",
            "field is missing in TS config, you must set it to:",
            showAsJsValue expected ++ "."
          ]
