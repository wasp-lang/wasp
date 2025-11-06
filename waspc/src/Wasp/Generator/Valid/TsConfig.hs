module Wasp.Generator.Valid.TsConfig
  ( validateSrcTsConfig,
  )
where

import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.Valid.Validator (Validation, failure, getValidationErrors, validateAll_, valueOfField, withFileName)

validateSrcTsConfig :: T.TsConfig -> [GeneratorError]
validateSrcTsConfig config =
  GenericGeneratorError . show
    <$> getValidationErrors validateFile config
  where
    -- References for understanding the required compiler options:
    --   - The comments in templates/sdk/wasp/tsconfig.json
    --   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
    --   - https://www.totaltypescript.com/tsconfig-cheat-sheet
    --   - https://www.typescriptlang.org/tsconfig/

    validateFile =
      withFileName "tsconfig.json" $
        validateAll_
          [ valueOfField ("include", T.include) $ eqJust ["src"],
            valueOfField ("compilerOptions", T.compilerOptions) validateCompilerOptions
          ]

    validateCompilerOptions =
      validateAll_
        [ valueOfField ("module", T._module) $ eqJust "esnext",
          valueOfField ("target", T.target) $ eqJust "esnext",
          -- Since Wasp ends up bundling the user code, `bundler` is the most
          -- appropriate `moduleResolution` option.
          valueOfField ("moduleResolution", T.moduleResolution) $ eqJust "bundler",
          valueOfField ("moduleDetection", T.moduleDetection) $ eqJust "force",
          -- `isolatedModules` prevents users from using features that don't work
          -- with transpilers and would fail when Wasp bundles the code with rollup
          -- (e.g., const enums)
          valueOfField ("isolatedModules", T.isolatedModules) $ eqJust True,
          valueOfField ("jsx", T.jsx) $ eqJust "preserve",
          valueOfField ("strict", T.strict) $ eqJust True,
          valueOfField ("esModuleInterop", T.esModuleInterop) $ eqJust True,
          valueOfField ("lib", T.lib) $ eqJust ["dom", "dom.iterable", "esnext"],
          valueOfField ("allowJs", T.allowJs) $ eqJust True,
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir`.
          valueOfField ("outDir", T.outDir) $ eqJust ".wasp/out/user",
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          valueOfField ("composite", T.composite) $ eqJust True,
          valueOfField ("skipLibCheck", T.skipLibCheck) $ eqJust True
        ]

eqJust :: (Eq a, Show a) => a -> Maybe a -> Validation ()
eqJust expected (Just actual)
  | actual == expected = pure ()
  | otherwise =
      failure $ "Expected " ++ show expected ++ " but got " ++ show actual ++ "."
eqJust expected Nothing =
  failure $ "Missing value, expected " ++ show expected ++ "."
