module Wasp.Generator.Valid.PackageJson.WaspConfig (waspConfigValidator) where

import Control.Monad (join)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Common (DependencySpecification, allCheckedDeps)
import qualified Wasp.Generator.Valid.Validator as V

waspConfigValidator :: AS.AppSpec -> V.Validator P.PackageJson
waspConfigValidator spec =
  V.inField ("wasp", P.wasp) $
    maybe V.success $
      V.all
        [ overriddenDepsValidator spec
        ]

overriddenDepsValidator :: AS.AppSpec -> V.Validator P.WaspConfig
overriddenDepsValidator spec =
  V.inField ("overriddenDeps", P.overriddenDeps) $
    forEachDep getValidationForDep . fromMaybe M.empty
  where
    overridableDeps = allCheckedDeps spec

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
forEachDep depValidator =
  join $ V.all . fmap eachDepValidator . M.toList
  where
    eachDepValidator dep@(pkgName, pkgVersion) =
      V.inField (pkgName, const pkgVersion) $ const $ depValidator dep
