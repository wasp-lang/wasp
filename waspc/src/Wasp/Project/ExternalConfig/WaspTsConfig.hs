module Wasp.Project.ExternalConfig.WaspTsConfig
  ( parseAndValidateWaspTsConfig,

    -- * Exported for testing only
    validateWaspTsConfig,
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
parseAndValidateWaspTsConfig = parseAndValidateTsConfigFile validateWaspTsConfig

validateWaspTsConfig :: T.TsConfig -> [CompileError]
validateWaspTsConfig config =
  show <$> V.execValidator waspTsConfigValidator config
  where
    waspTsConfigValidator :: V.Validator T.TsConfig
    waspTsConfigValidator =
      V.withFileName "tsconfig.wasp.json" $
        V.all
          [ V.inField ("include", T.include) $ V.eqJust ["main.wasp.ts"],
            V.inField ("compilerOptions", T.compilerOptions) $ V.required waspCompilerOptionsValidator
          ]

    waspCompilerOptionsValidator :: V.Validator T.CompilerOptions
    waspCompilerOptionsValidator =
      V.all
        [ V.inField ("target", T.target) $ V.eqJust "ES2022",
          V.inField ("module", T._module) $ V.eqJust "NodeNext",
          V.inField ("strict", T.strict) $ V.eqJust True,
          V.inField ("isolatedModules", T.isolatedModules) $ V.eqJust True,
          V.inField ("moduleDetection", T.moduleDetection) $ V.eqJust "force",
          V.inField ("skipLibCheck", T.skipLibCheck) $ V.eqJust True,
          V.inField ("noEmit", T.noEmit) $ V.eqJust True,
          V.inField ("lib", T.lib) $ V.eqJust ["ES2023"]
        ]
