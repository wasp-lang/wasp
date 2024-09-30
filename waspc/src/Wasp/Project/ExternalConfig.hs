module Wasp.Project.ExternalConfig
  ( analyzeExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import StrongPath (Abs, Dir, Path')
import qualified Wasp.ExternalConfig.PackageJson as P
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (analyzePackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (analyzeTsConfigFile)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: P.PackageJson,
    _tsConfig :: T.TsConfig
  }
  deriving (Show)

analyzeExternalConfigs :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] ExternalConfigs)
analyzeExternalConfigs waspDir = runExceptT $ do
  packageJsonContent <- ExceptT $ analyzePackageJsonFile waspDir
  tsConfigContent <- ExceptT $ analyzeTsConfigFile waspDir

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _tsConfig = tsConfigContent
      }
