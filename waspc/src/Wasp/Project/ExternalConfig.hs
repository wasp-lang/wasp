{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Project.ExternalConfig
  ( parseAndValidateExternalConfigs,
    ExternalConfigs (..),
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
import Wasp.Project.ExternalConfig.RootTsConfig (parseAndValidateRootTsConfig)
import Wasp.Project.ExternalConfig.SrcTsConfig (parseAndValidateSrcTsConfig)
import Wasp.Project.ExternalConfig.WaspTsConfig (parseAndValidateWaspTsConfig)
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
        <* sequenceA maybeWaspTsConfigOrErrors
        <* sequenceA maybeRootTsConfigOrErrors
        <* unitOrViteConfigErrors
