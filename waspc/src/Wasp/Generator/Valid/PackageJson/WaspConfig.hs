module Wasp.Generator.Valid.PackageJson.WaspConfig (waspConfigValidator) where

import qualified Data.Map as M
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Common (DependencySpecification, getAllWaspDependencies)
import qualified Wasp.Generator.Valid.Validator as V

waspConfigValidator :: AS.AppSpec -> V.Validator P.PackageJson
waspConfigValidator spec =
  V.inField ("wasp", P.wasp) $
    V.ifJust $
      V.all
        [ overriddenDepsValidator spec
        ]

overriddenDepsValidator :: AS.AppSpec -> V.Validator P.WaspConfig
overriddenDepsValidator spec =
  V.inField ("overriddenDeps", P.overriddenDeps) $
    V.ifJust $
      forEachDep getValidationForDep
  where
    overridableDeps :: M.Map P.PackageName P.PackageVersion
    overridableDeps = getAllWaspDependencies spec

    getValidationForDep (depName, actualVersion) =
      overridenVersionValidator
        (depName `M.lookup` overridableDeps)
        actualVersion

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

    unrecognizedDepError =
      V.failure
        "Wasp doesn't require this dependency, so there's no need to override it."

forEachDep :: V.Validator DependencySpecification -> V.Validator P.DependenciesMap
forEachDep depValidator depMap =
  -- Here we create a "two-pass" validator. First we examine the DependenciesMap
  -- to create a validator for each dep. Then, we call those validators with the
  -- same DependenciesMap to do the actual validation.
  (V.all $ eachDepValidator <$> M.toList depMap) depMap
  where
    eachDepValidator dep@(pkgName, _) =
      V.inField (pkgName, const dep) depValidator
