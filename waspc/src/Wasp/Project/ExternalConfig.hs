module Wasp.Project.ExternalConfig
  ( analyzeExternalConfigs,
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
import Wasp.Project.ExternalConfig.PackageJson (analyzePackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (analyzeSrcTsConfigFile)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: P.PackageJson,
    _tsConfig :: T.TsConfig
  }
  deriving (Show)

analyzeExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] ExternalConfigs)
analyzeExternalConfigs waspDir srcTsConfigFile = runExceptT $ do
  packageJsonContent <- ExceptT $ analyzePackageJsonFile waspDir
  tsConfigContent <- ExceptT $ analyzeSrcTsConfigFile waspDir srcTsConfigFile

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _tsConfig = tsConfigContent
      }
