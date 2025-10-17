{-# LANGUAGE FlexibleInstances #-}

module Wasp.Generator.Valid.TsConfig
  ( validateSrcTsConfig,
  )
where

import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.Monad (GeneratorError ())
import Wasp.Generator.Valid.Common (FullyQualifiedFieldName (FieldName), validateRequiredField)

validateSrcTsConfig :: T.TsConfig -> [GeneratorError]
validateSrcTsConfig srcTsConfig =
  validateRequiredField "tsconfig.json" (FieldName ["include"]) (T.include srcTsConfig) ["src"]
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
      validateRequiredField "tsconfig.json" (FieldName ["compilerOptions", relativeFieldName]) (getFieldValue compilerOptions)
