{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Project.ExternalConfig
  ( parseExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Maybe (catMaybes)
import StrongPath (Abs, Dir, Path')
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.ExternalConfig.TsConfig (TsConfig)
import Wasp.Project.Common
  ( CompileError,
    TsConfigPaths (..),
    WaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (readPackageJsonFile)
import Wasp.Project.ExternalConfig.TsConfig (findAndParseTsConfigFile, validateRootTsConfig, validateSrcTsConfig, validateWaspTsConfig)
import Wasp.Project.ExternalConfig.ViteConfig (validateViteConfig)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson,
    _srcTsConfig :: TsConfig,
    _rootTsConfig :: Maybe TsConfig,
    _waspTsConfig :: Maybe TsConfig
  }
  deriving (Show)

parseExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  TsConfigPaths ->
  IO (Either [CompileError] ExternalConfigs)
parseExternalConfigs waspDir tsConfigStructure = do
  readExternalConfigs waspDir tsConfigStructure >>= \case
    Left readError -> return $ Left [readError]
    Right externalConfigs ->
      case validateExternalConfigs externalConfigs of
        [] -> return $ Right externalConfigs
        errors -> return $ Left errors

readExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  TsConfigPaths ->
  IO (Either CompileError ExternalConfigs)
readExternalConfigs waspDir TsConfigPaths {srcTsConfig, waspTsConfig, rootTsConfig} = runExceptT $ do
  packageJsonContent <- ExceptT $ readPackageJsonFile waspDir
  srcTsConfigContent <- ExceptT $ findAndParseTsConfigFile waspDir srcTsConfig
  waspTsConfigContent <- traverse (ExceptT . findAndParseTsConfigFile waspDir) waspTsConfig
  rootTsConfigContent <- traverse (ExceptT . findAndParseTsConfigFile waspDir) rootTsConfig
  ExceptT $ validateViteConfig waspDir

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _srcTsConfig = srcTsConfigContent,
        _rootTsConfig = rootTsConfigContent,
        _waspTsConfig = waspTsConfigContent
      }

validateExternalConfigs :: ExternalConfigs -> [CompileError]
validateExternalConfigs configs =
  concat $
    catMaybes
      [ Just $ validateSrcTsConfig (_srcTsConfig configs),
        validateRootTsConfig <$> _rootTsConfig configs,
        validateWaspTsConfig <$> _waspTsConfig configs
      ]
