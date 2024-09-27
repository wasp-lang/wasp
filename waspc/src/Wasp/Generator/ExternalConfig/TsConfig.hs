{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.ExternalConfig.TsConfig
  ( validateTsConfig,
  )
where

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

type FieldName = String

validateTsConfig :: T.TsConfig -> [ErrorMsg]
validateTsConfig tsConfig =
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
      validateRequiredFieldInCompilerOptions "outDir" ".wasp/phantom" T.outDir
    ]
  where
    validateRequiredFieldInCompilerOptions fieldName expectedValue getFieldValue = case getFieldValue compilerOptionsFields of
      Just actualValue -> validateFieldValue ("compilerOptions." ++ fieldName) expectedValue actualValue
      Nothing -> [missingFieldErrorMessage]
      where
        missingFieldErrorMessage =
          unwords
            [ "The",
              show fieldName,
              "field is missing in tsconfig.json. Expected value:",
              showAsJsValue expectedValue ++ "."
            ]

    compilerOptionsFields = T.compilerOptions tsConfig

    validateFieldValue :: (Eq value, IsJavascriptValue value) => FieldName -> value -> value -> [String]
    validateFieldValue fieldName expectedValue actualValue =
      if actualValue == expectedValue
        then []
        else [invalidValueErrorMessage]
      where
        invalidValueErrorMessage =
          unwords
            [ "Invalid value for the",
              show fieldName,
              "field in tsconfig.json file, expected value:",
              showAsJsValue expectedValue ++ "."
            ]
