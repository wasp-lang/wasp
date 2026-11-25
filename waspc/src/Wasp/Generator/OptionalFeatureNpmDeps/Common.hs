module Wasp.Generator.OptionalFeatureNpmDeps.Common
  ( OptionalFeatureNpmDeps (..),
    getAllNpmDepsOfOptionalFeature,
    npmInstallCommands,
  )
where

import Wasp.AppSpec (AppSpec)
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency

-- | A Wasp feature and the npm packages that the generated code imports only
-- when the app uses it.
data OptionalFeatureNpmDeps = OptionalFeatureNpmDeps
  { -- | Completes the sentence "Your app uses ...".
    featureDescription :: String,
    isFeatureUsed :: AppSpec -> Bool,
    dependencies :: [Npm.Dependency.Dependency],
    devDependencies :: [Npm.Dependency.Dependency]
  }

getAllNpmDepsOfOptionalFeature :: OptionalFeatureNpmDeps -> [Npm.Dependency.Dependency]
getAllNpmDepsOfOptionalFeature optionalFeatureNpmDeps = dependencies optionalFeatureNpmDeps ++ devDependencies optionalFeatureNpmDeps

npmInstallCommands :: OptionalFeatureNpmDeps -> [String]
npmInstallCommands optionalFeatureNpmDeps =
  [makeInstallCommand [] deps | not (null deps)]
    ++ [makeInstallCommand ["--save-dev"] devDeps | not (null devDeps)]
  where
    deps = dependencies optionalFeatureNpmDeps
    devDeps = devDependencies optionalFeatureNpmDeps

    makeInstallCommand :: [String] -> [Npm.Dependency.Dependency] -> String
    makeInstallCommand flags packages = unwords $ "npm install" : flags ++ map makePackageSpec packages

    -- Some shells treat the `^` of a version range specially, so we quote it.
    makePackageSpec dep = "\"" ++ Npm.Dependency.name dep ++ "@" ++ Npm.Dependency.version dep ++ "\""
