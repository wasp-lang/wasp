module Wasp.Project.ExternalConfig
  ( readExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import StrongPath (Abs, Dir, File, Path', Rel)
import qualified Wasp.ExternalConfig.PackageJson as P
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common
  ( CompileError,
    SrcTsConfigFile,
    WaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (readPackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (readSrcTsConfigFile)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: P.PackageJson,
    _srcTsConfig :: T.TsConfig
  }
  deriving (Show)

readExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] ExternalConfigs)
readExternalConfigs waspDir srcTsConfigPath = runExceptT $ do
  packageJsonContent <- ExceptT $ readPackageJsonFile waspDir
  srcTsConfigContent <- ExceptT $ readSrcTsConfigFile waspDir srcTsConfigPath

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _srcTsConfig = srcTsConfigContent
      }
