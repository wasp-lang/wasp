module Wasp.Project.ExternalConfig.SrcTsConfig
  ( parseAndValidateSrcTsConfig,
    parseAndValidateModuleSrcTsConfig,

    -- * Exported for testing only
    srcTsConfigValidator,
    moduleSrcTsConfigValidator,
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

parseAndValidateModuleSrcTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateModuleSrcTsConfig = parseAndValidateTsConfigFile moduleSrcTsConfigValidator

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
  makeSrcTsConfigValidator
    (V.required $ V.containsAll ["src", ".wasp/out/types/app"])
    appCompilerOptionsValidator

moduleSrcTsConfigValidator :: V.Validator T.TsConfig
moduleSrcTsConfigValidator =
  makeSrcTsConfigValidator
    (V.required $ V.containsAll ["src", ".wasp/wasp/ambient.d.ts"])
    moduleCompilerOptionsValidator

makeSrcTsConfigValidator ::
  V.Validator (Maybe [String]) ->
  V.Validator T.CompilerOptions ->
  V.Validator T.TsConfig
makeSrcTsConfigValidator includeValidator compilerOptionsLayer =
  V.all
    [ V.inField ("include", T.include) includeValidator,
      V.inField ("exclude", T.exclude) $ V.required $ V.containsAll ["**/*.wasp.ts"],
      V.inField ("compilerOptions", T.compilerOptions) $
        V.required $
          V.all [commonCompilerOptionsValidator, compilerOptionsLayer]
    ]

commonCompilerOptionsValidator :: V.Validator T.CompilerOptions
commonCompilerOptionsValidator =
  V.all
    [ -- Since Wasp ends up bundling the user code, the module options must
      -- stay bundler-friendly. `esnext` also rejects CommonJS import syntax
      -- that would end up as an unresolved `require` in the ESM bundle.
      V.inField ("module", T._module) $ V.eqJust "esnext",
      V.inField ("moduleResolution", T.moduleResolution) $ V.eqJust "bundler",
      -- Without `moduleDetection: force`, TypeScript treats files with no
      -- imports or exports as global scripts, while the bundler treats them
      -- as modules. Code relying on such globals type checks but breaks at
      -- runtime after bundling.
      V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
      -- `isolatedModules` prevents users from using features that don't work
      -- with single-file transpilers and would fail at runtime after Wasp
      -- bundles the code (e.g., const enums).
      V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
      -- Bundlers emulate `esModuleInterop` behavior at runtime, so type
      -- checking must assume it too.
      V.inField ("esModuleInterop", T.esModuleInterop) $ V.eqJust True,
      -- From TypeScript 6 onwards, we need to manually specify which
      -- packages' globals we want to load.
      V.inField ("types", T.types) $ V.required $ V.containsAll ["react", "node"],
      V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True
    ]

appCompilerOptionsValidator :: V.Validator T.CompilerOptions
appCompilerOptionsValidator =
  V.all
    [ -- Both options match the automatic JSX transform esbuild applies when
      -- bundling.
      V.inField ("jsx", T.jsx) $ V.oneOfJust ["preserve", "react-jsx"],
      -- Wasp internally uses TypeScript's project references to compile the
      -- code. Referenced projects may not disable emit, so we must specify an
      -- `outDir` and keep `noEmit` off.
      V.inField ("outDir", T.outDir) $ V.eqJust ".wasp/out/user",
      V.inField ("noEmit", T.noEmit) $ V.ifJust $ V.eq False,
      -- The composite flag is required because Wasp uses project references
      -- (i.e., web app and server reference user code as a subproject)
      V.inField ("composite", T.composite) $ V.eqJust True
    ]

moduleCompilerOptionsValidator :: V.Validator T.CompilerOptions
moduleCompilerOptionsValidator =
  V.all
    [ -- Module source is prebuilt into plain .js files that host bundlers
      -- never transform, so JSX must be compiled away.
      V.inField ("jsx", T.jsx) $ V.eqJust "react-jsx",
      V.inField ("noEmit", T.noEmit) $ V.eqJust True
    ]
