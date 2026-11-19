module Wasp.ExternalConfig.Npm.PackageJson.DepValidators
  ( DependencyType (..),
    makeRequiredDepValidator,
    makeForbiddenDepValidator,
    inDependency,
  )
where

import qualified Data.Map as M
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Validator as V

data DependencyType = Runtime | Development deriving (Show)

-- | Validates that a required dependency is present in the correct dependency
-- list with the correct version. It shows an appropriate error message
-- otherwise (with an explicit check for the case when the dependency is present
-- in the opposite list -- runtime deps vs. devDeps).
makeRequiredDepValidator :: DependencyType -> (P.PackageName, P.PackageVersion) -> V.Validator P.PackageJson
makeRequiredDepValidator depType (pkgName, expectedPkgVersion) =
  inOppositeDepList notPresentValidator
    `V.and` inCorrectDepList correctVersionValidator
  where
    notPresentValidator :: V.Validator (Maybe P.PackageVersion)
    notPresentValidator Nothing = V.success
    notPresentValidator _ = wrongDepTypeError

    correctVersionValidator :: V.Validator (Maybe P.PackageVersion)
    correctVersionValidator (Just actualVersion)
      | actualVersion == expectedPkgVersion = V.success
    correctVersionValidator _ = incorrectPackageVersionError

    inCorrectDepList :: V.Validator (Maybe P.PackageVersion) -> V.Validator P.PackageJson
    inCorrectDepList = inDependency depType pkgName

    inOppositeDepList :: V.Validator (Maybe P.PackageVersion) -> V.Validator P.PackageJson
    inOppositeDepList = inDependency (oppositeForDepType depType) pkgName

    oppositeForDepType :: DependencyType -> DependencyType
    oppositeForDepType Runtime = Development
    oppositeForDepType Development = Runtime

    wrongDepTypeError =
      V.failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be in "
          ++ show (fst $ fieldForDepType depType)
          ++ "."

    incorrectPackageVersionError =
      V.failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " with version "
          ++ show expectedPkgVersion
          ++ "."

makeForbiddenDepValidator :: DependencyType -> P.PackageName -> V.Validator P.PackageJson
makeForbiddenDepValidator depType pkgName =
  inDependency depType pkgName forbiddenValidator
  where
    forbiddenValidator :: V.Validator (Maybe P.PackageVersion)
    forbiddenValidator Nothing = V.success
    forbiddenValidator _ = forbiddenError

    forbiddenError =
      V.failure $
        "Wasp doesn't allow a package named "
          ++ show pkgName
          ++ " to be present in "
          ++ show (fst $ fieldForDepType depType)
          ++ "."

-- | Runs the validator on a specific dependency of the given PackageJson
-- record, setting the appropriate path for errors.
inDependency ::
  DependencyType ->
  P.PackageName ->
  V.Validator (Maybe P.PackageVersion) ->
  V.Validator P.PackageJson
inDependency depType pkgName versionValidator =
  V.inField (fieldForDepType depType) $
    V.inField (pkgName, M.lookup pkgName) versionValidator

fieldForDepType :: DependencyType -> (String, P.PackageJson -> P.DependenciesMap)
fieldForDepType Runtime = ("dependencies", P.dependencies)
fieldForDepType Development = ("devDependencies", P.devDependencies)
