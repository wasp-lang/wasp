module Wasp.Project.ExternalConfig
  ( parseAndValidateExternalConfigs,
    ExternalConfigs (..),
  )
where

import Control.Arrow (left)
import StrongPath (Abs, Dir, Path')
import Validation (eitherToValidation, validationToEither)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.Project.Common
  ( CompileError,
    TsConfigPaths (..),
    WaspProjectDir,
  )
import Wasp.Project.ExternalConfig.PackageJson (parseUserPackageJson)
import Wasp.Project.ExternalConfig.RootTsConfig (parseAndValidateRootTsConfig)
import Wasp.Project.ExternalConfig.SrcTsConfig (parseAndValidateSrcTsConfig)
import Wasp.Project.ExternalConfig.ViteConfig (validateViteConfig)
import Wasp.Project.ExternalConfig.WaspTsConfig (parseAndValidateWaspTsConfig)
import Wasp.Project.PackageManager (PackageManager, detectPackageManager, showUnsupportedPackageManagerError)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: PackageJson,
    _packageManager :: PackageManager
  }
  deriving (Show)

parseAndValidateExternalConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  TsConfigPaths ->
  IO (Either [CompileError] ExternalConfigs)
parseAndValidateExternalConfigs waspDir TsConfigPaths {srcTsConfig, waspTsConfig, rootTsConfig} = do
  -- NOTE: We use Validation instead of Either because we don't want to fail
  -- early. We want to collect all validation errors.
  packageJsonOrErrors <- parseUserPackageJson waspDir
  srcTsConfigOrErrors <- parseAndValidateSrcTsConfig waspDir srcTsConfig
  maybeWaspTsConfigOrErrors <- traverse (parseAndValidateWaspTsConfig waspDir) waspTsConfig
  maybeRootTsConfigOrErrors <- traverse (parseAndValidateRootTsConfig waspDir) rootTsConfig
  unitOrViteConfigErrors <- validateViteConfig waspDir
  packageManagerOrErrors <- eitherToValidation . left (pure . showUnsupportedPackageManagerError) <$> detectPackageManager waspDir

  return
    $ validationToEither
    $ ExternalConfigs
      <$> packageJsonOrErrors
      <*> packageManagerOrErrors
      <* srcTsConfigOrErrors
      <* sequenceA maybeWaspTsConfigOrErrors
      <* sequenceA maybeRootTsConfigOrErrors
      <* unitOrViteConfigErrors
