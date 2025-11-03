module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Selective (whenS)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Validation as V
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.DepVersions (prismaVersion, typescriptVersion)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import qualified Wasp.Generator.NpmWorkspaces as NW
import Wasp.Generator.ServerGenerator.DepVersions (expressTypesVersion)
import Wasp.Generator.Valid.Validator
  ( Validation,
    failure,
    getValidationErrors,
    inField,
    validateAll_,
    withFileName,
  )
import qualified Wasp.Generator.WebAppGenerator.DepVersions as D

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson pkgJson =
  GenericGeneratorError . show
    <$> getValidationErrors validateFile pkgJson
  where
    validateFile =
      withFileName "package.json" $
        V.validateAll $
          [validateWorkspaces]
            <> validateRuntimeDeps
            <> validateDevDeps
            <> validateOptionalDeps

    validateRuntimeDeps =
      validateRequiredDependency Runtime
        <$> [ ("wasp", "file:.wasp/out/sdk/wasp"),
              ("react-router-dom", show D.reactRouterVersion),
              -- Installing the wrong version of "react-router-dom" can make users believe that they
              -- can use features that are not available in the version that Wasp supports.
              ("react", show D.reactVersion),
              ("react-dom", show D.reactVersion)
            ]

    validateDevDeps =
      validateRequiredDependency Development
        <$> [ ("vite", show D.viteVersion),
              ("prisma", show prismaVersion)
            ]

    validateOptionalDeps =
      validateOptionalDependency
        <$> [ ("typescript", show typescriptVersion),
              ("@types/react", show D.reactTypesVersion),
              ("@types/express", show expressTypesVersion)
            ]

-- | Validates that the 'workspaces' property in package.json includes all
-- expected workspaces used by Wasp.
validateWorkspaces :: P.PackageJson -> Validation ()
validateWorkspaces =
  inField "workspaces" P.workspaces $ \case
    Just workspaces -> allWorkspacesIncluded workspaces
    Nothing -> noWorkspacesPropertyError
  where
    allWorkspacesIncluded :: [String] -> Validation ()
    allWorkspacesIncluded =
      validateAll_ $ workspaceIncluded <$> expectedWorkspaces

    workspaceIncluded :: String -> [String] -> Validation ()
    workspaceIncluded expectedWorkspace actualWorkspaces
      | expectedWorkspace `elem` actualWorkspaces = pure ()
      | otherwise = makeMissingWorkspaceError expectedWorkspace

    makeMissingWorkspaceError expectedWorkspace =
      failure $
        "Wasp requires "
          ++ show expectedWorkspace
          ++ " to be included."

    noWorkspacesPropertyError =
      failure $
        "Wasp requires the value "
          ++ show expectedWorkspaces
          ++ "."

    expectedWorkspaces = S.toList NW.workspaceGlobs

-- | Validates that an optional dependency is either not present, or present
-- with the correct version. It does so in both regular and dev dependencies.
validateOptionalDependency :: PackageSpecification -> P.PackageJson -> Validation ()
validateOptionalDependency dep@(pkgName, expectedPkgVersion) =
  validateAll_ $
    [inDependency depType dep checkVersion | depType <- [Runtime, Development]]
  where
    checkVersion :: Maybe P.PackageVersion -> Validation ()
    checkVersion actualVersion =
      case (expectedPkgVersion ==) <$> actualVersion of
        Just True -> pure ()
        Just False -> incorrectVersionError
        Nothing -> pure ()

    incorrectVersionError =
      failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be version "
          ++ show expectedPkgVersion
          ++ " if present."

-- | Validates that a required dependency is present in the correct dependency
-- list with the correct version. It shows an appropriate error message
-- otherwise (with an explicit check for the case when the dependency is present
-- in the opposite list -- runtime deps vs. devDeps).
validateRequiredDependency :: DependencyType -> PackageSpecification -> P.PackageJson -> Validation ()
validateRequiredDependency depType dep@(pkgName, expectedPkgVersion) pkgJson =
  whenS (oppositeDep checkVersionNotPresent pkgJson) $
    correctDep checkCorrectVersion pkgJson
  where
    checkCorrectVersion :: Maybe P.PackageVersion -> Validation ()
    checkCorrectVersion actualVersion =
      case (expectedPkgVersion ==) <$> actualVersion of
        Just True -> pure ()
        Just False -> incorrectPackageVersionError
        Nothing -> missingPackageError

    checkVersionNotPresent :: Maybe P.PackageVersion -> Validation Bool
    checkVersionNotPresent Nothing = pure True
    checkVersionNotPresent _ = wrongDepTypeError

    correctDep :: (Maybe P.PackageVersion -> Validation a) -> P.PackageJson -> Validation a
    correctDep = inDependency depType dep
    oppositeDep :: (Maybe P.PackageVersion -> Validation a) -> P.PackageJson -> Validation a
    oppositeDep = inDependency (oppositeForDepType depType) dep

    oppositeForDepType :: DependencyType -> DependencyType
    oppositeForDepType Runtime = Development
    oppositeForDepType Development = Runtime

    wrongDepTypeError =
      failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be in "
          ++ show (fieldNameForDepType depType)
          ++ "."

    missingPackageError =
      failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " with version "
          ++ show expectedPkgVersion
          ++ "."

    incorrectPackageVersionError =
      failure $
        "Wasp requires package "
          ++ show pkgName
          ++ " to be version "
          ++ show expectedPkgVersion
          ++ "."

type PackageSpecification = (P.PackageName, P.PackageVersion)

-- | Runs the validator on a specific dependency of the input, setting the appropriate path for
-- errors.
inDependency ::
  DependencyType ->
  PackageSpecification ->
  (Maybe P.PackageVersion -> Validation a) ->
  P.PackageJson ->
  Validation a
inDependency depType (pkgName, _) innerValidator =
  inField (fieldNameForDepType depType) (getterForDepType depType) $
    inField pkgName (M.lookup pkgName) innerValidator

data DependencyType = Runtime | Development

fieldNameForDepType :: DependencyType -> String
fieldNameForDepType Runtime = "dependencies"
fieldNameForDepType Development = "devDependencies"

getterForDepType :: DependencyType -> P.PackageJson -> P.DependenciesMap
getterForDepType Runtime = P.dependencies
getterForDepType Development = P.devDependencies
