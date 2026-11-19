module Wasp.Generator.Valid.PackageJson.Dependencies
  ( dependenciesValidator,
    featureNpmDepsValidator, -- Exported for testing.
  )
where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.OptionalNpmDeps (FeatureNpmDeps, getNpmDepsOfUsedFeatures)
import qualified Wasp.Generator.OptionalNpmDeps as FeatureNpmDeps
import Wasp.Project.PackageManager (DependencyType (..), PackageManager, makeAddPackagesCommand)
import qualified Wasp.Validator as V

dependenciesValidator :: AS.AppSpec -> V.Validator P.PackageJson
dependenciesValidator spec = V.all $ featureNpmDepsValidator (AS.packageManager spec) <$> getNpmDepsOfUsedFeatures spec

-- | Checks that the project lists the packages of a feature the app uses. npm
-- checks their versions, because they are optional peer dependencies of
-- \@wasp/wasp.
featureNpmDepsValidator :: PackageManager -> FeatureNpmDeps -> V.Validator P.PackageJson
featureNpmDepsValidator packageManager featureNpmDeps packageJson
  | null missingDeps && null missingDevDeps = V.success
  | otherwise =
      V.failure $
        "Your app uses "
          ++ FeatureNpmDeps.featureName featureNpmDeps
          ++ ", so it needs "
          ++ intercalate ", " (Npm.Dependency.name <$> missingDeps ++ missingDevDeps)
          ++ ". Run "
          ++ intercalate " and " (map (\command -> "`" ++ command ++ "`") installCommands)
          ++ "."
  where
    missingDeps = filter isMissing $ FeatureNpmDeps.dependencies featureNpmDeps
    missingDevDeps = filter isMissing $ FeatureNpmDeps.devDependencies featureNpmDeps

    -- The user may list a package in either field.
    isMissing dep =
      not $ any (M.member $ Npm.Dependency.name dep) [P.dependencies packageJson, P.devDependencies packageJson]

    installCommands =
      [makeAddDepsCommand Runtime missingDeps | not (null missingDeps)]
        ++ [makeAddDepsCommand Development missingDevDeps | not (null missingDevDeps)]

    makeAddDepsCommand :: DependencyType -> [Npm.Dependency.Dependency] -> String
    makeAddDepsCommand dependencyType = makeAddPackagesCommand packageManager dependencyType . map makePackageSpecifier

    -- Quoted, because some shells treat the `^` of a version range specially.
    makePackageSpecifier dep = show $ Npm.Dependency.name dep ++ "@" ++ Npm.Dependency.version dep
