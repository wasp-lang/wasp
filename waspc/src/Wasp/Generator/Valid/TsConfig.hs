module Wasp.Generator.Valid.TsConfig
  ( validateSrcTsConfig,
  )
where

import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import qualified Wasp.Generator.Valid.Validator as V

validateSrcTsConfig :: T.TsConfig -> [GeneratorError]
validateSrcTsConfig config =
  GenericGeneratorError . show
    <$> V.execValidator tsConfigValidator config
  where
    -- References for understanding the required compiler options:
    --   - The comments in templates/sdk/wasp/tsconfig.json
    --   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
    --   - https://www.totaltypescript.com/tsconfig-cheat-sheet
    --   - https://www.typescriptlang.org/tsconfig/

    tsConfigValidator :: V.Validator T.TsConfig ()
    tsConfigValidator =
      V.withFileName "tsconfig.json" $
        V.all
          [ V.inField ("include", T.include) $ eqJust ["src"],
            V.inField ("compilerOptions", T.compilerOptions) compilerOptionsValidator
          ]

    compilerOptionsValidator :: V.Validator T.CompilerOptions ()
    compilerOptionsValidator =
      V.all
        [ V.inField ("module", T._module) $ eqJust "esnext",
          V.inField ("target", T.target) $ eqJust "esnext",
          -- Since Wasp ends up bundling the user code, `bundler` is the most
          -- appropriate `moduleResolution` option.
          V.inField ("moduleResolution", T.moduleResolution) $ eqJust "bundler",
          V.inField ("moduleDetection", T.moduleDetection) $ eqJust "force",
          -- `isolatedModules` prevents users from using features that don't work
          -- with transpilers and would fail when Wasp bundles the code with rollup
          -- (e.g., const enums)
          V.inField ("isolatedModules", T.isolatedModules) $ eqJust True,
          V.inField ("jsx", T.jsx) $ eqJust "preserve",
          V.inField ("strict", T.strict) $ eqJust True,
          V.inField ("esModuleInterop", T.esModuleInterop) $ eqJust True,
          V.inField ("lib", T.lib) $ eqJust ["dom", "dom.iterable", "esnext"],
          V.inField ("allowJs", T.allowJs) $ eqJust True,
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir`.
          V.inField ("outDir", T.outDir) $ eqJust ".wasp/out/user",
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          V.inField ("composite", T.composite) $ eqJust True,
          V.inField ("skipLibCheck", T.skipLibCheck) $ eqJust True
        ]

eqJust :: (Eq a, Show a) => a -> V.Validator' (Maybe a)
eqJust expected (Just actual)
  | actual == expected = pure ()
  | otherwise =
      V.failure $ "Expected " ++ show expected ++ " but got " ++ show actual ++ "."
eqJust expected Nothing =
  V.failure $ "Missing value, expected " ++ show expected ++ "."
