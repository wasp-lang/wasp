module Wasp.Generator.Valid.PackageJson.Dependencies
  ( dependenciesValidator,
    optionalFeatureNpmDepsValidator, -- Exported for testing.
  )
where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.ExternalConfig.Npm.PackageJson.DepValidators
  ( DependencyType (..),
    makeForbiddenDepValidator,
  )
import Wasp.Generator.OptionalFeatureNpmDeps (getNpmDepsOfUsedOptionalFeatures)
import Wasp.Generator.OptionalFeatureNpmDeps.Common (OptionalFeatureNpmDeps, npmInstallCommands)
import qualified Wasp.Generator.OptionalFeatureNpmDeps.Common as OptionalFeatureNpmDeps
import Wasp.Generator.Valid.PackageJson.Common (forbiddenUserDeps)
import qualified Wasp.Validator as V

dependenciesValidator :: AS.AppSpec -> V.Validator P.PackageJson
dependenciesValidator spec =
  V.all
    [ usedOptionalFeaturesNpmDepsValidator,
      forbiddenDepsValidator
    ]
  where
    usedOptionalFeaturesNpmDepsValidator :: V.Validator P.PackageJson
    usedOptionalFeaturesNpmDepsValidator = V.all $ optionalFeatureNpmDepsValidator <$> getNpmDepsOfUsedOptionalFeatures spec

    forbiddenDepsValidator :: V.Validator P.PackageJson
    forbiddenDepsValidator =
      V.all $
        [ makeForbiddenDepValidator depType pkgName
        | depType <- [Runtime, Development],
          pkgName <- forbiddenUserDeps
        ]

-- | Checks that the project lists the packages of a feature the app uses. The
-- peer dependencies already take care of their versions.
optionalFeatureNpmDepsValidator :: OptionalFeatureNpmDeps -> V.Validator P.PackageJson
optionalFeatureNpmDepsValidator optionalFeatureNpmDeps packageJson
  | null missingDeps && null missingDevDeps = V.success
  | otherwise =
      V.failure $
        "Your app uses "
          ++ OptionalFeatureNpmDeps.featureDescription optionalFeatureNpmDeps
          ++ ", so it needs "
          ++ intercalate ", " (Npm.Dependency.name <$> missingDeps ++ missingDevDeps)
          ++ ". Run "
          ++ intercalate " and " (map (\command -> "`" ++ command ++ "`") installCommands)
          ++ "."
  where
    missingDeps = filter isMissing $ OptionalFeatureNpmDeps.dependencies optionalFeatureNpmDeps
    missingDevDeps = filter isMissing $ OptionalFeatureNpmDeps.devDependencies optionalFeatureNpmDeps

    -- The user may list a package in either field.
    isMissing dep =
      not $ any (M.member $ Npm.Dependency.name dep) [P.dependencies packageJson, P.devDependencies packageJson]

    installCommands =
      npmInstallCommands
        optionalFeatureNpmDeps {OptionalFeatureNpmDeps.dependencies = missingDeps, OptionalFeatureNpmDeps.devDependencies = missingDevDeps}
