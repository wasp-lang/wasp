module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Selective (bindS)
import qualified Data.Map as M
import Validation (validateAll)
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.DepVersions (prismaVersion, typescriptVersion)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.ServerGenerator.DepVersions (expressTypesVersion)
import Wasp.Generator.Valid.Validator
  ( Validation,
    Validator,
    execValidator,
    failure,
    field,
    fileValidator,
  )
import qualified Wasp.Generator.WebAppGenerator.DepVersions as D

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson =
  (fmap (GenericGeneratorError . show) <$>) . execValidator $ do
    fileValidator "package.json" validateDependencies
  where
    validateDependencies =
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

validateOptionalDependency ::
  PackageSpecification ->
  Validator P.PackageJson ()
validateOptionalDependency dep@(pkgName, pkgVersion) pkgJson =
  forDependency Runtime dep validationForOptional pkgJson
    *> forDependency Development dep validationForOptional pkgJson
  where
    validationForOptional IncorrectVersion = incorrectVersionError
    validationForOptional _ = pure ()

    incorrectVersionError =
      failure $
        unwords
          [ "Wasp requires package",
            show pkgName,
            "to be version",
            show pkgVersion,
            "if present."
          ]

-- | Validates that a required dependency is present in the correct dependency
-- list with the correct version. It shows an appropriate error message
-- otherwise (with an explicit check for the case when the dependency is present
-- in the opposite list -- runtime deps vs. devDeps).
validateRequiredDependency ::
  DependencyType ->
  PackageSpecification ->
  Validator P.PackageJson ()
validateRequiredDependency depType dep@(pkgName, pkgVersion) pkgJson =
  -- `bindS` is the synonym of `>>=` for the `Selective` applicatives, that is,
  -- an effectul computation that depends on the previous one's result.
  --
  -- We use it here so that if we show the first error about the dependency
  -- being in the wrong place, it will short-circuit and not show the second
  -- error about it missing in in its correct place.
  bindS
    (forOppositeDepList resultForOpposite pkgJson)
    (const $ forCorrectDepList resultForCorrect pkgJson)
  where
    forCorrectDepList = forDependency depType dep
    forOppositeDepList = forDependency (oppositeDepType depType) dep

    oppositeDepType Runtime = Development
    oppositeDepType Development = Runtime

    resultForOpposite NotPresent = pure ()
    resultForOpposite _ = wrongDepTypeError

    resultForCorrect CorrectVersion = pure ()
    resultForCorrect NotPresent = missingPackageError
    resultForCorrect IncorrectVersion = incorrectPackageVersionError

    wrongDepTypeError =
      failure $
        unwords
          [ "Wasp requires package",
            show pkgName,
            "to be in",
            show (depTypeFieldName depType) ++ "."
          ]

    missingPackageError =
      failure $
        unwords
          [ "Wasp requires package",
            show pkgName,
            "with version",
            show pkgVersion ++ "."
          ]

    incorrectPackageVersionError =
      failure $
        unwords
          [ "Wasp requires package",
            show pkgName,
            "to be version",
            show pkgVersion ++ "."
          ]

type PackageSpecification = (P.PackageName, P.PackageVersion)

data DependencyType = Runtime | Development

forDependency ::
  DependencyType ->
  PackageSpecification ->
  (DependencyCheckResult -> Validation a) ->
  Validator P.PackageJson a
forDependency depType (pkgName, pkgVersion) f =
  field (depTypeFieldName depType) (depTypeGetter depType) $
    field pkgName (M.lookup pkgName) (f . checkDependency pkgVersion)
  where
    depTypeGetter Runtime = P.dependencies
    depTypeGetter Development = P.devDependencies

depTypeFieldName :: DependencyType -> String
depTypeFieldName Runtime = "dependencies"
depTypeFieldName Development = "devDependencies"

data DependencyCheckResult = CorrectVersion | IncorrectVersion | NotPresent
  deriving (Eq, Enum, Bounded)

checkDependency :: P.PackageVersion -> Maybe P.PackageVersion -> DependencyCheckResult
checkDependency _ Nothing = NotPresent
checkDependency expectedVersion (Just actualVersion)
  | expectedVersion == actualVersion = CorrectVersion
  | otherwise = IncorrectVersion
