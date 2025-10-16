module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Selective (bindS)
import Data.Foldable (traverse_)
import qualified Data.Map as M
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.DepVersions (prismaVersion, typescriptVersion)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.ServerGenerator.DepVersions (expressTypesVersion)
import Wasp.Generator.Valid.Common
  ( Validation,
    Validator,
    execValidator,
    failure,
    field,
    fileValidator,
    validateAll',
  )
import qualified Wasp.Generator.WebAppGenerator.DepVersions as D

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson =
  (fmap (GenericGeneratorError . show) <$>) . execValidator $ do
    fileValidator "package.json" validateDependencies
  where
    validateDependencies pkg =
      traverse_
        ($ pkg)
        ( fmap (validateRequiredDependency Runtime) requiredRuntimeDependencies
            <> fmap (validateRequiredDependency Development) requiredDevelopmentDependencies
            <> fmap validateOptionalDependency optionalDependencies
        )

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
  Validator P.PackageJson String ()
validateOptionalDependency dep@(pkgName, pkgVersion) =
  validateAll'
    [ forDependency Runtime dep validationForOptional,
      forDependency Development dep validationForOptional
    ]
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

validateRequiredDependency ::
  RequirementType ->
  PackageSpecification ->
  Validator P.PackageJson String ()
validateRequiredDependency reqType dep@(pkgName, pkgVersion) pkgJson =
  bindS
    (forWrongDependency validationForWrongPlace)
    (const $ forCorrectDependency validationForCorrectPlace)
  where
    validationForWrongPlace NotPresent = pure ()
    validationForWrongPlace _ = wrongDepTypeError

    validationForCorrectPlace CorrectVersion = pure ()
    validationForCorrectPlace NotPresent = missingPackageError
    validationForCorrectPlace IncorrectVersion = incorrectPackageVersionError

    forCorrectDependency check = forDependency reqType dep check pkgJson
    forWrongDependency check = forDependency (oppositeDepType reqType) dep check pkgJson

    wrongDepTypeError =
      failure $
        unwords
          [ "Wasp requires package",
            show pkgName,
            "to be in",
            show (requirementTypeName reqType) ++ "."
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

forDependency ::
  RequirementType ->
  (P.PackageName, P.PackageVersion) ->
  (DependencyCheckResult -> Validation error a) ->
  Validator P.PackageJson error a
forDependency kind (pkgName, pkgVersion) check =
  field (requirementTypeName kind) (requirementTypeGet kind) $
    field pkgName (M.lookup pkgName) (check . checkDependency pkgVersion)

oppositeDepType :: RequirementType -> RequirementType
oppositeDepType Runtime = Development
oppositeDepType Development = Runtime

requirementTypeGet :: RequirementType -> P.PackageJson -> P.DependenciesMap
requirementTypeGet Runtime = P.dependencies
requirementTypeGet Development = P.devDependencies

requirementTypeName :: RequirementType -> String
requirementTypeName Runtime = "dependencies"
requirementTypeName Development = "devDependencies"

data RequirementType = Runtime | Development

type PackageSpecification = (P.PackageName, P.PackageVersion)

checkDependency :: P.PackageVersion -> Maybe P.PackageVersion -> DependencyCheckResult
checkDependency expected (Just actual)
  | expected == actual = CorrectVersion
  | otherwise = IncorrectVersion
checkDependency _ Nothing = NotPresent

data DependencyCheckResult = CorrectVersion | IncorrectVersion | NotPresent
  deriving (Eq, Enum, Bounded)
