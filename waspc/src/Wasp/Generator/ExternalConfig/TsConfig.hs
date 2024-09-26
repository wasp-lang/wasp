{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.ExternalConfig.TsConfig
  ( validateTsConfig,
  )
where

import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common
  ( CompileError,
  )

validateTsConfig :: T.TsConfig -> IO (Either [CompileError] T.TsConfig)
validateTsConfig tsConfig =
  return $
    if null tsConfigErrors
      then Right tsConfig
      else Left tsConfigErrors
  where
    tsConfigErrors =
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

    validateRequiredFieldInCompilerOptions fieldName expectedValue getFieldValue = case getFieldValue compilerOptionsFields of
      Just actualValue -> validateFieldValue ("compilerOptions." ++ fieldName) expectedValue actualValue
      Nothing -> [missingFieldErrorMessage]
      where
        missingFieldErrorMessage = unwords ["The", show fieldName, "field is missing in tsconfig.json. Expected value:", showAsJsValue expectedValue ++ "."]

    compilerOptionsFields = T.compilerOptions tsConfig

-- | Haskell type that implements ShowJS is a type whose values can be mapped to Javascript values (their string representation).
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

validateFieldValue :: (Eq value, IsJavascriptValue value) => FieldName -> value -> value -> [CompileError]
validateFieldValue fieldName expectedValue actualValue =
  if actualValue == expectedValue
    then []
    else [invalidValueErrorMessage]
  where
    invalidValueErrorMessage = unwords ["Invalid value for the", show fieldName, "field in tsconfig.json file, expected value:", showAsJsValue expectedValue ++ "."]
