module Wasp.Generator.Valid.PackageJson.WaspConfig (waspConfigValidator) where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Common (DependencySpecification, getAllWaspDependencies)
import Wasp.Generator.WaspNpmPackageDeps (waspNpmPackageDeps)
import qualified Wasp.Validator as V

waspConfigValidator :: AS.AppSpec -> V.Validator P.PackageJson
waspConfigValidator spec =
  V.inField ("wasp", P.wasp)
    $ V.ifJust
    $ V.all
      [ overriddenDepsValidator spec
      ]

overriddenDepsValidator :: AS.AppSpec -> V.Validator P.WaspConfig
overriddenDepsValidator spec =
  V.inField ("overriddenDeps", P.overriddenDeps)
    $ V.ifJust
    $ forEachDep getValidationForDep
  where
    overridableDeps :: M.Map P.PackageName P.PackageVersion
    overridableDeps = getAllWaspDependencies spec

    getValidationForDep (depName, actualVersion)
      | depName `Set.member` waspNpmPackagePeerNames = peerDepOverrideError
      | otherwise =
          overridenVersionValidator
            (depName `M.lookup` overridableDeps)
            actualVersion

    waspNpmPackagePeerNames = Set.fromList $ map Npm.Dependency.name waspNpmPackageDeps

    overridenVersionValidator Nothing _ = unrecognizedDepError
    overridenVersionValidator (Just expectedVersion) actualVersion
      | actualVersion == expectedVersion = V.success
      | otherwise = incorrectDepVersionError expectedVersion actualVersion

    incorrectDepVersionError expectedVersion actualVersion =
      V.failure $
        "Wasp requires version "
          ++ expectedVersion
          ++ ", but found an override for version "
          ++ actualVersion
          ++ "."

    peerDepOverrideError =
      V.failure
        "@wasp/wasp declares this as a peer dependency, so npm enforces its version and overriding it here has no effect. Use npm's `overrides` field in package.json instead."

    unrecognizedDepError =
      V.failure
        "Wasp doesn't require this dependency, so there's no need to override it."

forEachDep :: V.Validator DependencySpecification -> V.Validator P.DependenciesMap
forEachDep depValidator depMap =
  traverse_ validateDep (M.toList depMap)
  where
    validateDep dep@(pkgName, _) =
      V.withFieldName pkgName $ depValidator dep
