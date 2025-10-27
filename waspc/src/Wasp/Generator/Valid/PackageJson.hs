module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Monad (void)
import qualified Data.Map as M
import Validation (validateAll)
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.DepVersions (prismaVersion, typescriptVersion)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.ServerGenerator.DepVersions (expressTypesVersion)
import Wasp.Generator.Valid.Validator (Validator, andThen, execValidator, failure, inField, inFile)
import qualified Wasp.Generator.WebAppGenerator.DepVersions as D

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson pkgJson =
  GenericGeneratorError . show
    <$> execValidator validateFile pkgJson
  where
    validateFile =
      inFile "package.json" $
        validateAll $
          (validateRequiredDependency Runtime <$> requiredRuntimeDependencies)
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

validateOptionalDependency :: PackageSpecification -> Validator P.PackageJson ()
validateOptionalDependency dep@(pkgName, pkgVersion) =
  void
    . validateAll
      [ inDependency Runtime dep (resultForOptional . checkVersion),
        inDependency Development dep (resultForOptional . checkVersion)
      ]
  where
    checkVersion = eqVersion pkgVersion

    resultForOptional IncorrectVersion = incorrectVersionError
    resultForOptional _ = pure ()

    incorrectVersionError =
      failure $
        "Wasp requires package " ++ show pkgName ++ " to be version " ++ show pkgVersion ++ " if present."

-- | Validates that a required dependency is present in the correct dependency
-- list with the correct version. It shows an appropriate error message
-- otherwise (with an explicit check for the case when the dependency is present
-- in the opposite list -- runtime deps vs. devDeps).
validateRequiredDependency :: DependencyType -> PackageSpecification -> Validator P.PackageJson ()
validateRequiredDependency depType dep@(pkgName, pkgVersion) pkgJson =
  inOppositeDepList (resultForOpposite . checkVersion) pkgJson
    `andThen` inCorrectDepList (resultForCorrect . checkVersion) pkgJson
  where
    checkVersion = eqVersion pkgVersion

    inCorrectDepList = inDependency depType dep
    inOppositeDepList = inDependency (oppositeDepType depType) dep

    oppositeDepType Runtime = Development
    oppositeDepType Development = Runtime

    resultForOpposite NotPresent = pure ()
    resultForOpposite _ = wrongDepTypeError

    resultForCorrect CorrectVersion = pure ()
    resultForCorrect NotPresent = missingPackageError
    resultForCorrect IncorrectVersion = incorrectPackageVersionError

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
  DependencyType -> PackageSpecification -> Validator (Maybe P.PackageVersion) a -> Validator P.PackageJson a
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

eqVersion :: P.PackageVersion -> Maybe P.PackageVersion -> DependencyCheckResult
eqVersion _ Nothing = NotPresent
eqVersion expectedVersion (Just actualVersion)
  | expectedVersion == actualVersion = CorrectVersion
  | otherwise = IncorrectVersion

data DependencyCheckResult = CorrectVersion | IncorrectVersion | NotPresent
