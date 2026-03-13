module Wasp.Project.ExternalConfig.RootTsConfig
  ( parseAndValidateRootTsConfig,

    -- * Exported for testing only
    validateRootTsConfig,
  )
where

import Data.Bool (bool)
import StrongPath (Abs, Dir, File, Path', Rel)
import Validation (Validation (..))
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, RootTsConfigFile, WaspProjectDir)
import Wasp.Project.ExternalConfig.TsConfig (parseAndValidateTsConfigFile)
import qualified Wasp.Validator as V

parseAndValidateRootTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File RootTsConfigFile) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateRootTsConfig = parseAndValidateTsConfigFile validateRootTsConfig

-- TODO: remove hardcoded and duplicated paths paths
validateRootTsConfig :: T.TsConfig -> [CompileError]
validateRootTsConfig config =
  show <$> V.execValidator rootTsConfigValidator config
  where
    rootTsConfigValidator :: V.Validator T.TsConfig
    rootTsConfigValidator =
      V.withFileName "tsconfig.json" $
        V.all
          [ V.inField ("files", T.files) $ V.eqJust [],
            V.inField ("references", T.references) $
              V.required $
                V.all $ makeReferenceIncludedValidator <$> requiredReferences
          ]

    requiredReferences :: [String]
    requiredReferences = ["./tsconfig.src.json", "./tsconfig.wasp.json"]

    makeReferenceIncludedValidator :: String -> V.Validator [T.TsConfigReference]
    makeReferenceIncludedValidator expectedPath =
      bool missingReferenceError V.success
        . elem expectedPath
        . fmap T.path
      where
        missingReferenceError =
          V.failure $
            "Missing required reference { \"path\": " ++ show expectedPath ++ " }."
