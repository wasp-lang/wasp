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

data PackageValidationDependecyType = RuntimeDependency | DevelopmentDependency

data PackageValidationOptionality = Required | Optional

type PackageValidationType = (PackageValidationDependecyType, PackageValidationOptionality)

validatePackageJson :: P.PackageJson -> [ErrorMsg]
validatePackageJson packageJson =
  concat
    [ -- dependencies
      validate ("wasp", "file:.wasp/out/sdk/wasp") (RuntimeDependency, Required),
      validate ("react-router-dom", show reactRouterVersion) (RuntimeDependency, Required),
      -- Installing the wrong version of "react-router-dom" can make users believe that they
      -- can use features that are not available in the version that Wasp supports.
      validate ("react", show reactVersion) (RuntimeDependency, Required),
      validate ("react-dom", show reactVersion) (RuntimeDependency, Required),
      -- devDependencies
      validate ("typescript", show typescriptVersion) (DevelopmentDependency, Optional),
      validate ("vite", show viteVersion) (DevelopmentDependency, Required),
      validate ("prisma", show prismaVersion) (DevelopmentDependency, Required),
      validate ("@types/react", show reactTypesVersion) (DevelopmentDependency, Optional),
      validate ("@types/express", show expressTypesVersion) (DevelopmentDependency, Optional)
    ]
  where
    validate = validatePackageJsonDependency packageJson

validatePackageJsonDependency :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> PackageValidationType -> [ErrorMsg]
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
      RuntimeDependency -> P.dependencies packageJson
      DevelopmentDependency -> P.devDependencies packageJson

    packageJsonDependencyKey :: String
    packageJsonDependencyKey = case dependencyType of
      RuntimeDependency -> "dependencies"
      DevelopmentDependency -> "devDependencies"

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
