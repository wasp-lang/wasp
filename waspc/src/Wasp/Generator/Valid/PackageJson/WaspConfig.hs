module Wasp.Generator.Valid.PackageJson.WaspConfig (waspConfigValidator) where

import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Common (DependencySpecification, getGeneratedServerDeps)
import Wasp.NpmPackageNames (waspNpmPackageName)
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
    overridableDeps = getGeneratedServerDeps spec

    getValidationForDep (depName, actualVersion) =
      overridenVersionValidator
        (depName `M.lookup` overridableDeps)
        actualVersion

    overridenVersionValidator Nothing _ = notOverridableDepError
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

    notOverridableDepError =
      V.failure $
        "`wasp.overriddenDeps` only takes the dependencies of the generated server, which `wasp deps` lists. For a package that comes with "
          ++ waspNpmPackageName
          ++ ", use npm's `overrides` field in package.json instead."

forEachDep :: V.Validator DependencySpecification -> V.Validator P.DependenciesMap
forEachDep depValidator depMap =
  traverse_ validateDep (M.toList depMap)
  where
    validateDep dep@(pkgName, _) =
      V.withFieldName pkgName $ depValidator dep
