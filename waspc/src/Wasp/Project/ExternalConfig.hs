{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Project.ExternalConfig
  ( parseAndValidateExternalConfigs,
    ExternalConfigs (..),

    -- * Exported for testing only
    validateSrcTsConfig,
  )
where

import StrongPath (Abs, Dir, Path')
import qualified Validation as V
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
    V.validationToEither $
      ExternalConfigs
        <$> V.eitherToValidation packageJsonOrErrors
        <* V.eitherToValidation srcTsConfigOrErrors
        <* V.eitherToValidation (sequence maybeWaspTsConfigOrErrors)
        <* V.eitherToValidation (sequence maybeRootTsConfigOrErrors)
        <* V.eitherToValidation unitOrViteConfigErrors
