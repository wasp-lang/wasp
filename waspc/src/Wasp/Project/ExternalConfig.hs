module Wasp.Project.ExternalConfig
  ( analyzeExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import StrongPath (Abs, Dir, File, Path', Rel)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.ExternalConfig.TsConfig (TsConfig)
import Wasp.Project.Common
  ( CompileError,
    SrcTsConfigFile,
    WaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (analyzePackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (analyzeSrcTsConfigFile)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson,
    _tsConfig :: TsConfig,
    _srcTsConfigPath :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile)
  }
  deriving (Show)

analyzeExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] ExternalConfigs)
analyzeExternalConfigs waspDir srcTsConfigPath = runExceptT $ do
  packageJsonContent <- ExceptT $ analyzePackageJsonFile waspDir
  tsConfigContent <- ExceptT $ analyzeSrcTsConfigFile waspDir srcTsConfigPath

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _tsConfig = tsConfigContent,
        _srcTsConfigPath = srcTsConfigPath
      }
