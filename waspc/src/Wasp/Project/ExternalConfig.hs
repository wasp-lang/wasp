module Wasp.Project.ExternalConfig
  ( readExternalConfigs,
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
import Wasp.Project.ExternalConfig.PackageJson (readPackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (readSrcTsConfigFile)
import Wasp.Project.ExternalConfig.ViteConfig (validateViteConfig)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson,
    _srcTsConfig :: TsConfig
  }
  deriving (Show)

readExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either CompileError ExternalConfigs)
readExternalConfigs waspDir srcTsConfigPath = runExceptT $ do
  packageJsonContent <- ExceptT $ readPackageJsonFile waspDir
  srcTsConfigContent <- ExceptT $ readSrcTsConfigFile waspDir srcTsConfigPath
  ExceptT $ validateViteConfig waspDir

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _srcTsConfig = srcTsConfigContent
      }
