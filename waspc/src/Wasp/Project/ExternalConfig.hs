module Wasp.Project.ExternalConfig
  ( parseExternalConfigs,
    ExternalConfigs (..),

    -- * Exported for testing only
    validateSrcTsConfig,
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
import Wasp.Project.ExternalConfig.TsConfig (readSrcTsConfigFile, validateSrcTsConfig)
import Wasp.Project.ExternalConfig.ViteConfig (validateViteConfig)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson,
    _srcTsConfig :: TsConfig
  }
  deriving (Show)

parseExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [CompileError] ExternalConfigs)
parseExternalConfigs waspDir srcTsConfigPath = do
  readExternalConfigs waspDir srcTsConfigPath >>= \case
    Left readError -> return $ Left [readError]
    Right externalConfigs ->
      case validateExternalConfigs externalConfigs of
        [] -> return $ Right externalConfigs
        errors -> return $ Left errors

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

validateExternalConfigs :: ExternalConfigs -> [CompileError]
validateExternalConfigs configs =
  validateSrcTsConfig (_srcTsConfig configs)
