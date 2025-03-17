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

type PackageValidation = (PackageDependecyType, PackageRequirement)

validatePackageJson :: P.PackageJson -> [ErrorMsg]
validatePackageJson packageJson =
  concat
    [ -- dependencies
      validate ("wasp", "file:.wasp/out/sdk/wasp") (Runtime, Required),
      validate ("react-router-dom", show reactRouterVersion) (Runtime, Required),
      -- Installing the wrong version of "react-router-dom" can make users believe that they
      -- can use features that are not available in the version that Wasp supports.
      validate ("react", show reactVersion) (Runtime, Required),
      validate ("react-dom", show reactVersion) (Runtime, Required),
      -- devDependencies
      validate ("typescript", show typescriptVersion) (Development, Optional),
      validate ("vite", show viteVersion) (Development, Required),
      validate ("prisma", show prismaVersion) (Development, Required),
      validate ("@types/react", show reactTypesVersion) (Development, Optional),
      validate ("@types/express", show expressTypesVersion) (Development, Optional)
    ]
  where
    validate = validatePackageJsonDependency packageJson

validatePackageJsonDependency :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> PackageValidation -> [ErrorMsg]
validatePackageJsonDependency packageJson (packageName, expectedPackageVersion) (dependencyType, optionality) =
  case M.lookup packageName packageJsonDependencesMap of
    Just actualPackageVersion ->
      if actualPackageVersion == expectedPackageVersion
        then []
        else [incorrectPackageVersionErrorMessage]
    _notListed -> case optionality of
      Required -> [missingRequiredPackageErrorMessage]
      Optional -> []
  where
    packageJsonDependencesMap :: P.DependenciesMap
    packageJsonDependencesMap = case dependencyType of
      Runtime -> P.dependencies packageJson
      Development -> P.devDependencies packageJson

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
