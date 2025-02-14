module Wasp.Project.ExternalConfig
  ( analyzeExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import StrongPath (File, Path', Rel)
import qualified Wasp.ExternalConfig.PackageJson as P
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common
  ( CompileError,
    SrcTsConfigFile,
    WaspProjectDir,
  )
import qualified Wasp.Project.ExternalConfig.ExternalConfigAnalysisContext as ECC
import Wasp.Project.ExternalConfig.PackageJson (analyzePackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (analyzeSrcTsConfigFile)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: P.PackageJson,
    _tsConfig :: T.TsConfig,
    _srcTsConfigPath :: Path' (Rel WaspProjectDir) (File SrcTsConfigFile)
  }
  deriving (Show)

analyzeExternalConfigs ::
  ECC.ExternalConfigAnalysisContext ->
  IO (Either [CompileError] ExternalConfigs)
analyzeExternalConfigs context = runExceptT $ do
  packageJsonContent <- ExceptT $ analyzePackageJsonFile context
  tsConfigContent <- ExceptT $ analyzeSrcTsConfigFile context

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _tsConfig = tsConfigContent,
        _srcTsConfigPath = ECC._srcTsConfigPath context
      }
