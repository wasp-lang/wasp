module Wasp.Project.ExternalConfig.RootTsConfig
  ( parseAndValidateRootTsConfig,

    -- * Exported for testing only
    rootTsConfigValidator,
  )
where

import Data.Bool (bool)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', Rel, fromRelFile)
import qualified System.FilePath as FP
import Validation (Validation (..))
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, RootTsConfigFile, TsConfigPaths (..), WaspProjectDir, tsConfigPaths)
import Wasp.Project.ExternalConfig.TsConfig (parseAndValidateTsConfigFile)
import qualified Wasp.Validator as V

parseAndValidateRootTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File RootTsConfigFile) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateRootTsConfig = parseAndValidateTsConfigFile rootTsConfigValidator

rootTsConfigValidator :: V.Validator T.TsConfig
rootTsConfigValidator =
  V.all
    [ V.inField ("files", T.files) $ V.eqJust [],
      V.inField ("references", T.references) $
        V.required $
          V.all $
            makeReferenceIncludedValidator <$> requiredReferences
    ]
  where
    requiredReferences :: [String]
    requiredReferences =
      [ fromRelFile tsConfigPaths.srcTsConfig,
        fromRelFile $ fromJust tsConfigPaths.waspTsConfig
      ]

    makeReferenceIncludedValidator :: String -> V.Validator [T.TsConfigReference]
    makeReferenceIncludedValidator expectedPath =
      bool missingReferenceError V.success
        . any (FP.equalFilePath expectedPath . T.path)
      where
        missingReferenceError =
          V.failure $
            "Missing required reference { \"path\": " ++ show expectedPath ++ " }."
