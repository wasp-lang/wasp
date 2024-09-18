module Wasp.ExternalConfig
  ( ExternalConfigs (..),
    analyzeExternalConfigs,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import StrongPath (Abs, Dir, Path')
import Wasp.ExternalConfig.PackageJson (PackageJson, analyzePackageJsonContent)
import Wasp.ExternalConfig.TsConfig (TsConfig, analyzeTsConfigContent)
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
  )

data ExternalConfigs = ExternalConfigs
  { packageJson :: PackageJson,
    tsConfig :: TsConfig
  }
  deriving (Show)

analyzeExternalConfigs :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] ExternalConfigs)
analyzeExternalConfigs waspDir = runExceptT $ do
  packageJsonContent <- ExceptT $ analyzePackageJsonContent waspDir
  tsConfigContent <- ExceptT $ analyzeTsConfigContent waspDir

  return $
    ExternalConfigs
      { packageJson = packageJsonContent,
        tsConfig = tsConfigContent
      }
