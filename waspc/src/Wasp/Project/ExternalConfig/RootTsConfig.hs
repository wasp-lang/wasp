module Wasp.Project.ExternalConfig.RootTsConfig
  ( parseAndValidateRootTsConfig,

    -- * Exported for testing only
    validateRootTsConfig,
  )
where

import Data.Bool (bool)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, File, Path', Rel, fromRelFile)
import Validation (Validation (..))
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, RootTsConfigFile, TsConfigPaths (..), WaspProjectDir, tsConfigPathsInWaspTsProjects)
import Wasp.Project.ExternalConfig.TsConfig (parseAndValidateTsConfigFile)
import qualified Wasp.Validator as V

parseAndValidateRootTsConfig ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File RootTsConfigFile) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateRootTsConfig = parseAndValidateTsConfigFile validateRootTsConfig

validateRootTsConfig :: String -> T.TsConfig -> [CompileError]
validateRootTsConfig tsConfigFileName tsConfigContents =
  show <$> V.execValidator tsConfigValidator tsConfigContents
  where
    tsConfigValidator :: V.Validator T.TsConfig
    tsConfigValidator =
      V.withFileName tsConfigFileName $
        V.all
          [ V.inField ("files", T.files) $ V.eqJust [],
            V.inField ("references", T.references) $
              V.required $
                V.all $
                  makeReferenceIncludedValidator <$> requiredReferences
          ]

    requiredReferences :: [String]
    requiredReferences =
      [ fromRelFile tsConfigPathsInWaspTsProjects.srcTsConfig,
        fromRelFile $ fromJust tsConfigPathsInWaspTsProjects.waspTsConfig
      ]

    makeReferenceIncludedValidator :: String -> V.Validator [T.TsConfigReference]
    makeReferenceIncludedValidator expectedPath =
      bool missingReferenceError V.success
        . elem expectedPath
        . fmap T.path
      where
        missingReferenceError =
          V.failure $
            "Missing required reference { \"path\": " ++ show expectedPath ++ " }."
