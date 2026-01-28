module Wasp.Generator.Valid.PackageJson.Dependencies
  ( dependenciesValidator,

    -- * Exported for testing only
    makeOptionalDepValidator,
    makeRequiredDepValidator,
    inDependency,
    DependencyType (..),
  )
where

import qualified Data.Map as M
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.DepVersions
  ( expressTypesVersion,
    prismaVersion,
    reactDomTypesVersion,
    reactDomVersion,
    reactRouterVersion,
    reactTypesVersion,
    reactVersion,
    typescriptVersion,
    viteVersion,
  )
import qualified Wasp.Generator.Valid.Validator as V

data DependencyType = Runtime | Development deriving (Show)

type DependencySpecification = (P.PackageName, P.PackageVersion)

requiredRuntimeDeps :: [DependencySpecification]
requiredRuntimeDeps =
  [ -- Installing the wrong version of "react-router" can make users believe that they
    -- can use features that are not available in the version that Wasp supports.
    ("react-router", show reactRouterVersion),
    ("react", show reactVersion),
    ("react-dom", show reactDomVersion)
  ]

requiredDevelopmentDeps :: [DependencySpecification]
requiredDevelopmentDeps =
  [ ("vite", show viteVersion),
    ("prisma", show prismaVersion)
  ]

optionalDeps :: [DependencySpecification]
optionalDeps =
  [ ("typescript", show typescriptVersion),
    ("@types/react", show reactTypesVersion),
    ("@types/react-dom", show reactDomTypesVersion),
    ("@types/express", show expressTypesVersion)
  ]

forbiddenDeps :: [P.PackageName]
forbiddenDeps =
  [ -- The `wasp` package is used in the `workspaces` field of the user's package.json.
    -- It shouldn't be listed as a dependency so it's not overwritten.
    "wasp"
  ]

dependenciesValidator :: V.Validator P.PackageJson
dependenciesValidator =
  V.all
    [ runtimeDepsValidator,
      developmentDepsValidator,
      optionalDepsValidator,
      forbiddenDepsValidator
    ]
  where
    runtimeDepsValidator :: V.Validator P.PackageJson
    runtimeDepsValidator =
      V.all $ makeRequiredDepValidator Runtime <$> requiredRuntimeDeps

    developmentDepsValidator :: V.Validator P.PackageJson
    developmentDepsValidator =
      V.all $ makeRequiredDepValidator Development <$> requiredDevelopmentDeps

    optionalDepsValidator :: V.Validator P.PackageJson
    optionalDepsValidator =
      V.all $
        [ makeOptionalDepValidator depType dep
          | depType <- [Runtime, Development],
            dep <- optionalDeps
        ]

    forbiddenDepsValidator :: V.Validator P.PackageJson
    forbiddenDepsValidator =
      V.all $
        [ makeForbiddenDepValidator depType pkgName
          | depType <- [Runtime, Development],
            pkgName <- forbiddenDeps
        ]

-- | Validates that a required dependency is present in the correct dependency
-- list with the correct version. It shows an appropriate error message
-- otherwise (with an explicit check for the case when the dependency is present
-- in the opposite list -- runtime deps vs. devDeps).
makeRequiredDepValidator :: DependencyType -> DependencySpecification -> V.Validator P.PackageJson
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

-- | Validates that an optional dependency is either not present, or present
-- with the correct version.
makeOptionalDepValidator :: DependencyType -> DependencySpecification -> V.Validator P.PackageJson
makeOptionalDepValidator depType (pkgName, expectedPkgVersion) =
  inDependency depType pkgName optionalVersionValidator
  where
    optionalVersionValidator :: V.Validator (Maybe P.PackageVersion)
    optionalVersionValidator (Just actualVersion)
      | actualVersion /= expectedPkgVersion = incorrectVersionError
    optionalVersionValidator _ = V.success

    incorrectVersionError =
      V.failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be version "
          ++ show expectedPkgVersion
          ++ " if present."

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
