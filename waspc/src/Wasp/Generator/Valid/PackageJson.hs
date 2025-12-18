module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,

    -- * Exported for testing only
    makeOptionalDepValidator,
    makeRequiredDepValidator,
    inDependency,
    DependencyType (..),
  )
where

import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
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
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import qualified Wasp.Generator.NpmWorkspaces as NW
import qualified Wasp.Generator.Valid.Validator as V

data DependencyType = Runtime | Development
  deriving (Show)

type DependencySpecification = (P.PackageName, P.PackageVersion)

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson pkgJson =
  GenericGeneratorError . show
    <$> V.execValidator packageJsonValidator pkgJson
  where
    packageJsonValidator :: V.Validator P.PackageJson
    packageJsonValidator =
      V.withFileName "package.json" $
        V.all
          [ workspacesValidator,
            dependenciesValidator
          ]

type WorkspaceName = String

workspacesValidator :: V.Validator P.PackageJson
workspacesValidator =
  V.inField ("workspaces", P.workspaces) $ \case
    Just actualWorkspaces -> requiredWorkspacesIncludedValidator actualWorkspaces
    Nothing -> workspacesNotDefinedError
  where
    requiredWorkspacesIncludedValidator :: V.Validator [WorkspaceName]
    requiredWorkspacesIncludedValidator =
      V.all $ makeWorskpaceIncludedValidator <$> expectedWorkspaces

    makeWorskpaceIncludedValidator :: WorkspaceName -> V.Validator [WorkspaceName]
    makeWorskpaceIncludedValidator expectedWorkspace actualWorkspaces
      | expectedWorkspace `elem` actualWorkspaces = V.success
      | otherwise = makeMissingWorkspaceError expectedWorkspace

    makeMissingWorkspaceError expectedWorkspace =
      V.failure $
        "Wasp requires "
          ++ show expectedWorkspace
          ++ " to be included."

    workspacesNotDefinedError =
      V.failure $
        "Wasp requires the field to be an array, and include the following values: "
          ++ intercalate ", " (show <$> expectedWorkspaces)
          ++ "."

    expectedWorkspaces = S.toList NW.requiredWorkspaceGlobs

dependenciesValidator :: V.Validator P.PackageJson
dependenciesValidator =
  V.all
    [ runtimeDepsValidator,
      developmentDepsValidator,
      optionalDepsValidator
    ]
  where
    runtimeDepsValidator :: V.Validator P.PackageJson
    runtimeDepsValidator =
      V.all $
        makeRequiredDepValidator Runtime
          <$> [ ("wasp", "file:.wasp/out/sdk/wasp"),
                -- Installing the wrong version of "react-router-dom" can make users believe that they
                -- can use features that are not available in the version that Wasp supports.
                ("react-router-dom", show reactRouterVersion),
                ("react", show reactVersion),
                ("react-dom", show reactDomVersion)
              ]

    developmentDepsValidator :: V.Validator P.PackageJson
    developmentDepsValidator =
      V.all $
        makeRequiredDepValidator Development
          <$> [ ("vite", show viteVersion),
                ("prisma", show prismaVersion)
              ]

    optionalDepsValidator :: V.Validator P.PackageJson
    optionalDepsValidator =
      V.all $
        [ makeOptionalDepValidator depType dep
          | depType <- [Runtime, Development],
            dep <-
              [ ("typescript", show typescriptVersion),
                ("@types/react", show reactTypesVersion),
                ("@types/react-dom", show reactDomTypesVersion),
                ("@types/express", show expressTypesVersion)
              ]
        ]

-- | Validates that an optional dependency is either not present, or present
-- with the correct version.
makeOptionalDepValidator :: DependencyType -> DependencySpecification -> V.Validator P.PackageJson
makeOptionalDepValidator depType (pkgName, expectedPkgVersion) =
  inDependency depType pkgName optionalVersionValidator
  where
    optionalVersionValidator :: V.Validator (Maybe P.PackageVersion)
    optionalVersionValidator actualVersion =
      case (expectedPkgVersion ==) <$> actualVersion of
        Just True -> V.success
        Just False -> incorrectVersionError
        Nothing -> V.success

    incorrectVersionError =
      V.failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be version "
          ++ show expectedPkgVersion
          ++ " if present."

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
    correctVersionValidator actualVersion =
      case (expectedPkgVersion ==) <$> actualVersion of
        Just True -> V.success
        Just False -> incorrectPackageVersionError
        Nothing -> missingPackageError

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

    missingPackageError =
      V.failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " with version "
          ++ show expectedPkgVersion
          ++ "."

    incorrectPackageVersionError =
      V.failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be version "
          ++ show expectedPkgVersion
          ++ "."

-- | Runs the validator on a specific dependency of the given PackageJson
-- record, setting the appropriate path for errors.
inDependency ::
  DependencyType ->
  P.PackageName ->
  V.Validator (Maybe P.PackageVersion) ->
  V.Validator P.PackageJson
inDependency depType pkgName versionStringValidator =
  V.inField (fieldForDepType depType) $
    V.inField (pkgName, M.lookup pkgName) versionStringValidator

fieldForDepType :: DependencyType -> (String, P.PackageJson -> P.DependenciesMap)
fieldForDepType Runtime = ("dependencies", P.dependencies)
fieldForDepType Development = ("devDependencies", P.devDependencies)
