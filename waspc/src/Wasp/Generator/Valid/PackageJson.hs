module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Selective (ifS)
import qualified Data.Map as M
import Data.Maybe (isNothing)
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
            <> (validateRequiredDependency Runtime <$> requiredRuntimeDependencies)
            <> (validateRequiredDependency Development <$> requiredDevelopmentDependencies)
            <> (validateOptionalDependency <$> optionalDependencies)

    requiredRuntimeDependencies =
      [ ("wasp", "file:.wasp/out/sdk/wasp"),
        ("react-router-dom", show D.reactRouterVersion),
        -- Installing the wrong version of "react-router-dom" can make users believe that they
        -- can use features that are not available in the version that Wasp supports.
        ("react", show D.reactVersion),
        ("react-dom", show D.reactVersion)
      ]

    requiredDevelopmentDependencies =
      [ ("vite", show D.viteVersion),
        ("prisma", show prismaVersion)
      ]

    optionalDependencies =
      [ ("typescript", show typescriptVersion),
        ("@types/react", show D.reactTypesVersion),
        ("@types/express", show expressTypesVersion)
      ]

validateWorkspaces :: P.PackageJson -> Validation ()
validateWorkspaces =
  inField "workspaces" P.workspaces $
    maybe missingWorkspacesKeyError validateWorkspaceValues
  where
    validateWorkspaceValues =
      validateAll_ $ validateSpecificWorkspace <$> S.toList NW.workspaceGlobs

    validateSpecificWorkspace expectedWorkspace actualWorkspaces
      | expectedWorkspace `elem` actualWorkspaces = pure ()
      | otherwise = makeMissingSpecificWorkspaceError expectedWorkspace

    makeMissingSpecificWorkspaceError expectedWorkspace =
      failure $
        "Wasp requires "
          ++ show expectedWorkspace
          ++ " to be included."

    missingWorkspacesKeyError =
      failure $
        "Wasp requires the value "
          ++ show (S.toList NW.workspaceGlobs)
          ++ "."

validateOptionalDependency :: PackageSpecification -> P.PackageJson -> Validation ()
validateOptionalDependency dep@(pkgName, pkgVersion) =
  validateAll_
    [ inDependency Runtime dep checkVersion,
      inDependency Development dep checkVersion
    ]
  where
    checkVersion version = case (pkgVersion ==) <$> version of
      Just True -> pure ()
      Just False -> incorrectVersionError
      Nothing -> pure ()

    incorrectVersionError =
      failure $
        "Wasp requires package " ++ show pkgName ++ " to be version " ++ show pkgVersion ++ " if present."

-- | Validates that a required dependency is present in the correct dependency
-- list with the correct version. It shows an appropriate error message
-- otherwise (with an explicit check for the case when the dependency is present
-- in the opposite list -- runtime deps vs. devDeps).
validateRequiredDependency :: DependencyType -> PackageSpecification -> P.PackageJson -> Validation ()
validateRequiredDependency depType dep@(pkgName, pkgVersion) pkgJson =
  (ifS $ oppositeDep eqNothing pkgJson)
    (correctDep checkVersion pkgJson)
    wrongDepTypeError
  where
    eqNothing = pure . isNothing

    checkVersion version =
      case (pkgVersion ==) <$> version of
        Just True -> pure ()
        Just False -> incorrectPackageVersionError
        Nothing -> missingPackageError

    correctDep = inDependency depType dep
    oppositeDep = inDependency (oppositeDepType depType) dep

    oppositeDepType Runtime = Development
    oppositeDepType Development = Runtime

    wrongDepTypeError =
      failure $
        "Wasp requires package " ++ show pkgName ++ " to be in " ++ show (depTypeFieldName depType) ++ "."

    missingPackageError =
      failure $
        "Wasp requires package " ++ show pkgName ++ " with version " ++ show pkgVersion ++ "."

    incorrectPackageVersionError =
      failure $
        "Wasp requires package " ++ show pkgName ++ " to be version " ++ show pkgVersion ++ "."

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
  inField (depTypeFieldName depType) (depTypeGetter depType) $
    inField pkgName (M.lookup pkgName) innerValidator

data DependencyType = Runtime | Development

depTypeFieldName :: DependencyType -> String
depTypeFieldName Runtime = "dependencies"
depTypeFieldName Development = "devDependencies"

depTypeGetter :: DependencyType -> P.PackageJson -> P.DependenciesMap
depTypeGetter Runtime = P.dependencies
depTypeGetter Development = P.devDependencies
