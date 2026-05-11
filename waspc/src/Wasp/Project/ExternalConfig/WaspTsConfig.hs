module Wasp.Project.ExternalConfig.WaspTsConfig
  ( parseAndValidateWaspTsConfig,

    -- * Exported for testing only
    waspTsConfigValidator,
  )
where

import StrongPath (Abs, Dir, File, Path', Rel)
import Validation (Validation (..))
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, WaspProjectDir, WaspTsConfigFile)
import Wasp.Project.ExternalConfig.TsConfig (parseAndValidateTsConfigFile)
import qualified Wasp.Validator as V

parseAndValidateWaspTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File WaspTsConfigFile) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateWaspTsConfig = parseAndValidateTsConfigFile waspTsConfigValidator

waspTsConfigValidator :: V.Validator T.TsConfig
waspTsConfigValidator =
  V.all
    [ V.inField ("include", T.include) $ V.eqJust ["main.wasp.ts"],
      V.inField ("compilerOptions", T.compilerOptions) $ V.required compilerOptionsValidator
    ]
  where
    compilerOptionsValidator :: V.Validator T.CompilerOptions
    compilerOptionsValidator =
      V.all
        [ V.inField ("target", T.target) $ V.eqJust "ES2022",
          V.inField ("module", T._module) $ V.eqJust "NodeNext",
          V.inField ("strict", T.strict) $ V.eqJust True,
          V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
          V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
          V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True,
          V.inField ("noEmit", T.noEmit) $ V.eqJust True,
          V.inField ("lib", T.lib) $ V.eqJust ["ES2023"],
          -- TypeScript 6 stopped auto-including `@types/*` from `node_modules`,
          -- so we require Node's types here. main.wasp.ts is Node code and may
          -- reference `process`, `console`, etc.
          V.inField ("types", T.types) $ V.containsAllJust ["node"]
        ]
