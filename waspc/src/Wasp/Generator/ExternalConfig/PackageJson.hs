module Wasp.Generator.ExternalConfig.PackageJson
  ( validatePackageJson,
  )
where

import qualified Data.Map as M
import qualified Wasp.ExternalConfig.PackageJson as P
import Wasp.Generator.Common (prismaVersion, typescriptVersion)
import Wasp.Generator.ExternalConfig.Common (ErrorMsg)
import Wasp.Generator.ServerGenerator.Common (expressTypesVersion)
import Wasp.Generator.WebAppGenerator.Common (reactRouterVersion, reactTypesVersion, reactVersion, viteVersion)

data PackageDependecyType = Runtime | Development

data PackageRequirement = Required | Optional

type PackageConstraints = (PackageDependecyType, PackageRequirement)

type PackageSpecification = (P.PackageName, P.PackageVersion)

validatePackageJson :: P.PackageJson -> [ErrorMsg]
validatePackageJson packageJson =
  validateRuntimeDependencies packageJson ++ validateDevelopmentDependencies packageJson

validateRuntimeDependencies :: P.PackageJson -> [ErrorMsg]
validateRuntimeDependencies packageJson =
  concat
    [ validateRuntime ("wasp", "file:.wasp/out/sdk/wasp") Required,
      validateRuntime ("react-router-dom", show reactRouterVersion) Required,
      -- Installing the wrong version of "react-router-dom" can make users believe that they
      -- can use features that are not available in the version that Wasp supports.
      validateRuntime ("react", show reactVersion) Required,
      validateRuntime ("react-dom", show reactVersion) Required
    ]
  where
    validateRuntime pkg req = validatePackageJsonDependency packageJson pkg (Runtime, req)

validateDevelopmentDependencies :: P.PackageJson -> [ErrorMsg]
validateDevelopmentDependencies packageJson =
  concat
    [ validateDevelopment ("typescript", show typescriptVersion) Optional,
      validateDevelopment ("vite", show viteVersion) Required,
      validateDevelopment ("prisma", show prismaVersion) Required,
      validateDevelopment ("@types/react", show reactTypesVersion) Optional,
      validateDevelopment ("@types/express", show expressTypesVersion) Optional
    ]
  where
    validateDevelopment pkg req = validatePackageJsonDependency packageJson pkg (Development, req)

validatePackageJsonDependency :: P.PackageJson -> PackageSpecification -> PackageConstraints -> [ErrorMsg]
validatePackageJsonDependency packageJson (packageName, expectedPackageVersion) (dependencyType, optionality) =
  case getPackageJsonDependency of
    Just actualPackageVersion ->
      if actualPackageVersion == expectedPackageVersion
        then []
        else [incorrectPackageVersionErrorMessage]
    _notListed ->
      if isInWrongDependencyType
        then [wrongDependencyTypeErrorMessage]
        else case optionality of
          Required -> [missingRequiredPackageErrorMessage]
          Optional -> []
  where
    getPackageJsonDependency :: Maybe P.PackageVersion
    getPackageJsonDependency = case dependencyType of
      Runtime -> M.lookup packageName $ P.dependencies packageJson
      Development -> M.lookup packageName $ P.devDependencies packageJson

    isInWrongDependencyType :: Bool
    isInWrongDependencyType = case dependencyType of
      Runtime -> M.member packageName (P.devDependencies packageJson)
      Development -> M.member packageName (P.dependencies packageJson)

    wrongPackageJsonDependencyKey :: String
    wrongPackageJsonDependencyKey = case dependencyType of
      Runtime -> "devDependencies"
      Development -> "dependencies"

    packageJsonDependencyKey :: String
    packageJsonDependencyKey = case dependencyType of
      Runtime -> "dependencies"
      Development -> "devDependencies"

    incorrectPackageVersionErrorMessage :: ErrorMsg
    incorrectPackageVersionErrorMessage =
      unwords
        [ "Wasp requires package",
          show packageName,
          "to be version",
          show expectedPackageVersion,
          "in package.json."
        ]

    missingRequiredPackageErrorMessage :: ErrorMsg
    missingRequiredPackageErrorMessage =
      unwords
        [ "Wasp requires package",
          show packageName,
          "with version",
          show expectedPackageVersion,
          "to be in",
          packageJsonDependencyKey,
          "in package.json."
        ]

    wrongDependencyKeTyperorMessage :: ErrorMsg
    wrongDependencyKTyperrorMessage =
      unwords
        [ "Wasp requires package",
          show packageName,
          "to be in",
          packageJsonDependencyKey,
          "instead of",
          wrongPackageJsonDependencyKey,
          "in package.json."
        ]
