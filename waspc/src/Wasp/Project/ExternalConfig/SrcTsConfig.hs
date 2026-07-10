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

-- References for understanding the required compiler options:
--   - The comments in templates/sdk/wasp/tsconfig.json
--   - https://www.typescriptlang.org/docs/handbook/modules/introduction.html
--   - https://www.totaltypescript.com/tsconfig-cheat-sheet
--   - https://www.typescriptlang.org/tsconfig/
srcTsConfigValidator :: V.Validator T.TsConfig
srcTsConfigValidator =
  makeSrcTsConfigValidator
    (V.eqJust ["src"])
    appCompilerOptionsValidator

moduleSrcTsConfigValidator :: V.Validator T.TsConfig
moduleSrcTsConfigValidator =
  makeSrcTsConfigValidator
    (V.eqJust ["src", ".wasp/wasp/ambient.d.ts"])
    moduleCompilerOptionsValidator

makeSrcTsConfigValidator ::
  V.Validator (Maybe [String]) ->
  V.Validator T.CompilerOptions ->
  V.Validator T.TsConfig
makeSrcTsConfigValidator includeValidator compilerOptionsLayer =
  V.all
    [ V.inField ("include", T.include) includeValidator,
      V.inField ("exclude", T.exclude) $ V.eqJust ["**/*.wasp.ts"],
      V.inField ("compilerOptions", T.compilerOptions) $
        V.required $
          V.all [commonCompilerOptionsValidator, compilerOptionsLayer]
    ]

commonCompilerOptionsValidator :: V.Validator T.CompilerOptions
commonCompilerOptionsValidator =
  V.all
    [ V.inField ("module", T._module) $ V.eqJust "esnext",
      V.inField ("target", T.target) $ V.eqJust "esnext",
      -- Since Wasp ends up bundling source code, `bundler` is the most
      -- appropriate `moduleResolution` option.
      V.inField ("moduleResolution", T.moduleResolution) $ V.eqJust "bundler",
      V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
      -- `isolatedModules` prevents users from using features that don't work
      -- with Wasp's transpilers (e.g., const enums).
      V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
      V.inField ("jsx", T.jsx) $ V.eqJust "preserve",
      V.inField ("strict", T.strict) $ V.eqJust True,
      V.inField ("esModuleInterop", T.esModuleInterop) $ V.eqJust True,
      V.inField ("lib", T.lib) $ V.eqJust ["dom", "dom.iterable", "esnext"],
      -- From TypeScript 6 onwards, we need to manually specify which
      -- packages' globals we want to load.
      V.inField ("types", T.types) $ V.required $ V.containsAll ["react", "node"],
      V.inField ("allowJs", T.allowJs) $ V.eqJust True,
      V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True
    ]

appCompilerOptionsValidator :: V.Validator T.CompilerOptions
appCompilerOptionsValidator =
  V.all
    [ -- Wasp uses project references to compile user code. Referenced projects
      -- may not disable emit, so they need a dedicated output directory.
      V.inField ("outDir", T.outDir) $ V.eqJust ".wasp/out/user",
      V.inField ("composite", T.composite) $ V.eqJust True
    ]

moduleCompilerOptionsValidator :: V.Validator T.CompilerOptions
moduleCompilerOptionsValidator =
  V.inField ("noEmit", T.noEmit) $ V.eqJust True
