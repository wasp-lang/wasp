module Wasp.Project.ExternalConfig.SrcTsConfig
  ( parseAndValidateSrcTsConfig,

    -- * Exported for testing only
    srcTsConfigValidator,
  )
where

import StrongPath (Abs, Dir, File, Path', Rel)
import Validation (Validation (..))
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, SrcTsConfigFile, WaspProjectDir)
import Wasp.Project.ExternalConfig.TsConfig (parseAndValidateTsConfigFile)
import qualified Wasp.Validator as V

parseAndValidateSrcTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateSrcTsConfig = parseAndValidateTsConfigFile srcTsConfigValidator

-- Wasp only requires the options it needs to compile and bundle the project.
-- Everything else (strictness, target, lib, ...) is the user's choice.
-- We ensure proper defaults through starter templates.
--
-- References for understanding the required compiler options:
--   - The comments in templates/sdk/wasp/tsconfig.json
--   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
--   - https://www.totaltypescript.com/tsconfig-cheat-sheet
--   - https://www.typescriptlang.org/tsconfig/
srcTsConfigValidator :: V.Validator T.TsConfig
srcTsConfigValidator =
  V.all
    [ V.inField ("include", T.include) $ V.required $ V.containsAll ["src", ".wasp/out/types/app"],
      V.inField ("exclude", T.exclude) $ V.required $ V.containsAll ["**/*.wasp.ts"],
      V.inField ("compilerOptions", T.compilerOptions) $ V.required compilerOptionsValidator
    ]
  where
    compilerOptionsValidator :: V.Validator T.CompilerOptions
    compilerOptionsValidator =
      V.all
        [ -- Since Wasp ends up bundling the user code, the module options must
          -- stay bundler-friendly.
          V.inField ("module", T._module) $ V.oneOfJust ["esnext", "preserve"],
          V.inField ("moduleResolution", T.moduleResolution) $ V.eqJust "bundler",
          isolatedModulesValidator,
          -- Both options match the automatic JSX transform esbuild applies when
          -- bundling.
          V.inField ("jsx", T.jsx) $ V.oneOfJust ["preserve", "react-jsx"],
          -- Bundlers emulate `esModuleInterop` behavior at runtime, so type
          -- checking must assume it too.
          V.inField ("esModuleInterop", T.esModuleInterop) $ V.eqJust True,
          -- From TypeScript 6 onwards, we need to manually specify which
          -- packages' globals we want to load.
          V.inField ("types", T.types) $ V.required $ V.containsAll ["react", "node"],
          -- Wasp internally uses TypeScript's project references to compile the
          -- code. Referenced projects may not disable emit, so we must specify an
          -- `outDir` and keep `noEmit` off.
          V.inField ("outDir", T.outDir) $ V.eqJust ".wasp/out/user",
          V.inField ("noEmit", T.noEmit) $ V.ifJust $ V.eq False,
          -- The composite flag is required because Wasp uses project references
          -- (i.e., web app and server reference user code as a subproject)
          V.inField ("composite", T.composite) $ V.eqJust True,
          V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True
        ]

    -- `isolatedModules` prevents users from using features that don't work with
    -- single-file transpilers and would fail at runtime after Wasp bundles the code
    -- (e.g., const enums).
    -- `verbatimModuleSyntax` is a stricter alternative that gives the same guarantee.
    isolatedModulesValidator :: V.Validator T.CompilerOptions
    isolatedModulesValidator compilerOptions
      | T.isolatedModules compilerOptions == Just True = V.success
      | T.verbatimModuleSyntax compilerOptions == Just True = V.success
      | otherwise =
          V.withFieldName "isolatedModules" $
            V.failure "Expected \"isolatedModules\" (or the stricter \"verbatimModuleSyntax\") to be true."
