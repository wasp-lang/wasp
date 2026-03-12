{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Project.ExternalConfig
  ( parseAndValidateExternalConfigs,
    ExternalConfigs (..),

    -- * Exported for testing only
    validateSrcTsConfig,
  )
where

import StrongPath (Abs, Dir, Path')
import Validation (validationToEither)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.Project.Common
  ( CompileError,
    TsConfigPaths (..),
    WaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (parseAndValidateUserPackageJson)
import Wasp.Project.ExternalConfig.TsConfig (parseAndValidateRootTsConfig, parseAndValidateSrcTsConfig, parseAndValidateWaspTsConfig, validateSrcTsConfig)
import Wasp.Project.ExternalConfig.ViteConfig (validateViteConfig)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson
  }
  deriving (Show)

parseAndValidateExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  TsConfigPaths ->
  IO (Either [CompileError] ExternalConfigs)
parseAndValidateExternalConfigs waspDir TsConfigPaths {srcTsConfig, waspTsConfig, rootTsConfig} = do
  packageJsonOrErrors <- parseAndValidateUserPackageJson waspDir
  srcTsConfigOrErrors <- parseAndValidateSrcTsConfig waspDir srcTsConfig
  maybeWaspTsConfigOrErrors <- traverse (parseAndValidateWaspTsConfig waspDir) waspTsConfig
  maybeRootTsConfigOrErrors <- traverse (parseAndValidateRootTsConfig waspDir) rootTsConfig
  unitOrViteConfigErrors <- validateViteConfig waspDir

  return $
    validationToEither $
      ExternalConfigs
        <$> packageJsonOrErrors
        <* srcTsConfigOrErrors
        <* sequence maybeWaspTsConfigOrErrors
        <* sequence maybeRootTsConfigOrErrors
        <* unitOrViteConfigErrors
