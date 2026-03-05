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

    tsConfigValidator :: V.Validator T.TsConfig
    tsConfigValidator =
      V.withFileName "tsconfig.json" $
        V.all
          [ V.inField ("include", T.include) $ V.eqJust ["src"],
            V.inField ("compilerOptions", T.compilerOptions) compilerOptionsValidator
          ]

    compilerOptionsValidator :: V.Validator T.CompilerOptions
    compilerOptionsValidator =
      V.all
        [ V.inField ("module", T._module) $ V.eqJust "esnext",
          V.inField ("target", T.target) $ V.eqJust "esnext",
          -- Since Wasp ends up bundling the user code, `bundler` is the most
          -- appropriate `moduleResolution` option.
          V.inField ("moduleResolution", T.moduleResolution) $ V.eqJust "bundler",
          V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
          -- `isolatedModules` prevents users from using features that don't work
          -- with transpilers and would fail when Wasp bundles the code with rollup
          -- (e.g., const enums)
          V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
          V.inField ("jsx", T.jsx) $ V.eqJust "preserve",
          V.inField ("strict", T.strict) $ V.eqJust True,
          V.inField ("esModuleInterop", T.esModuleInterop) $ V.eqJust True,
          V.inField ("lib", T.lib) $ V.eqJust ["dom", "dom.iterable", "esnext"],
          V.inField ("allowJs", T.allowJs) $ V.eqJust True,
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir`.
          V.inField ("outDir", T.outDir) $ V.eqJust ".wasp/out/user",
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          V.inField ("composite", T.composite) $ V.eqJust True,
          V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True
        ]
