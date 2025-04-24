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
  -- References for understanding the required compiler options:
  --   - The comments in templates/sdk/wasp/tsconfig.json
  --   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
  --   - https://www.totaltypescript.com/tsconfig-cheat-sheet
  --   - https://www.typescriptlang.org/tsconfig/
  concat
    [ validateRequiredFieldInCompilerOptions "module" T._module "esnext",
      validateRequiredFieldInCompilerOptions "target" T.target "esnext",
      -- Since Wasp ends up bundling the user code, `bundler` is the most
      -- appropriate `moduleResolution` option.
      validateRequiredFieldInCompilerOptions "moduleResolution" T.moduleResolution "bundler",
      validateRequiredFieldInCompilerOptions "moduleDetection" T.moduleDetection "force",
      -- `isolatedModules` prevents users from using features that don't work
      -- with transpilers and would fail when Wasp bundles the code with rollup
      -- (e.g., const enums)
      validateRequiredFieldInCompilerOptions "isolatedModules" T.isolatedModules True,
      validateRequiredFieldInCompilerOptions "jsx" T.jsx "preserve",
      validateRequiredFieldInCompilerOptions "strict" T.strict True,
      validateRequiredFieldInCompilerOptions "esModuleInterop" T.esModuleInterop True,
      validateRequiredFieldInCompilerOptions "lib" T.lib ["dom", "dom.iterable", "esnext"],
      validateRequiredFieldInCompilerOptions "allowJs" T.allowJs True,
      -- Wasp internally uses TypeScript's project references to compile the
      -- code. Referenced projects may not disable emit, so we must specify an
      -- `outDir`.
      validateRequiredFieldInCompilerOptions "outDir" T.outDir ".wasp/out/user",
      -- The composite flag is required because Wasp uses project references
      -- (i.e., web app and server reference user code as a subproject)
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
