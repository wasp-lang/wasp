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
  concat
    [ validateRequiredFieldInCompilerOptions "module" "esnext" T._module,
      validateRequiredFieldInCompilerOptions "target" "esnext" T.target,
      validateRequiredFieldInCompilerOptions "moduleResolution" "bundler" T.moduleResolution,
      validateRequiredFieldInCompilerOptions "jsx" "preserve" T.jsx,
      validateRequiredFieldInCompilerOptions "strict" True T.strict,
      validateRequiredFieldInCompilerOptions "esModuleInterop" True T.esModuleInterop,
      validateRequiredFieldInCompilerOptions "lib" ["dom", "dom.iterable", "esnext"] T.lib,
      validateRequiredFieldInCompilerOptions "allowJs" True T.allowJs,
      validateRequiredFieldInCompilerOptions "typeRoots" ["node_modules/@testing-library", "node_modules/@types"] T.typeRoots,
      validateRequiredFieldInCompilerOptions "outDir" ".wasp/out/user" T.outDir
    ]
  where
    validateRequiredFieldInCompilerOptions fieldName expectedValue getFieldValue = case fieldValue of
      Just actualValue -> validateFieldValue fullyQualifiedFieldName expectedValue actualValue
      Nothing -> [missingFieldErrorMessage]
      where
        fieldValue = getFieldValue $ T.compilerOptions tsConfig
        fullyQualifiedFieldName = FieldName ["compilerOptions", fieldName]

        missingFieldErrorMessage =
          unwords
            [ "The",
              "\"" ++ show fullyQualifiedFieldName ++ "\"",
              "field is missing in tsconfig.json, expected value:",
              showAsJsValue expectedValue ++ "."
            ]

    validateFieldValue :: (Eq value, IsJavascriptValue value) => FullyQualifiedFieldName -> value -> value -> [String]
    validateFieldValue fullyQualifiedFieldName expectedValue actualValue =
      if actualValue == expectedValue
        then []
        else [invalidValueErrorMessage]
      where
        invalidValueErrorMessage =
          unwords
            [ "Invalid value for the",
              "\"" ++ show fullyQualifiedFieldName ++ "\"",
              "field in tsconfig.json, expected value:",
              showAsJsValue expectedValue ++ "."
            ]
