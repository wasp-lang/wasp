module Wasp.Generator.ExternalConfig.PackageJson
  ( validatePackageJson,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import qualified Wasp.ExternalConfig.PackageJson as P
import Wasp.Generator.Common (prismaVersion, typescriptVersion)
import Wasp.Generator.ExternalConfig.Common (ErrorMsg)
import Wasp.Generator.ServerGenerator.Common (expressTypesVersion)
import Wasp.Generator.WebAppGenerator.Common (reactRouterVersion, reactTypesVersion, reactVersion, viteVersion)

data PackageRequirement
  = RequiredRuntime
  | RequiredDevelopment
  | Optional

type PackageSpecification = (P.PackageName, P.PackageVersion)

validatePackageJson :: P.PackageJson -> [ErrorMsg]
validatePackageJson packageJson =
  validateRuntimeDependencies packageJson
    ++ validateDevelopmentDependencies packageJson
    ++ validateOptionalDependencies packageJson

validateRuntimeDependencies :: P.PackageJson -> [ErrorMsg]
validateRuntimeDependencies packageJson =
  concat
    [ validateRuntime ("wasp", "file:.wasp/out/sdk/wasp"),
      validateRuntime ("react-router-dom", show reactRouterVersion),
      -- Installing the wrong version of "react-router-dom" can make users believe that they
      -- can use features that are not available in the version that Wasp supports.
      validateRuntime ("react", show reactVersion),
      validateRuntime ("react-dom", show reactVersion)
    ]
  where
    validateRuntime spec = validatePackageJsonDependency packageJson spec RequiredRuntime

validateDevelopmentDependencies :: P.PackageJson -> [ErrorMsg]
validateDevelopmentDependencies packageJson =
  concat
    [ validateDevelopment ("vite", show viteVersion),
      validateDevelopment ("prisma", show prismaVersion)
    ]
  where
    validateDevelopment spec = validatePackageJsonDependency packageJson spec RequiredDevelopment

validateOptionalDependencies :: P.PackageJson -> [ErrorMsg]
validateOptionalDependencies packageJson =
  concat
    [ validateOptional ("typescript", show typescriptVersion),
      validateOptional ("@types/react", show reactTypesVersion),
      validateOptional ("@types/express", show expressTypesVersion)
    ]
  where
    validateOptional spec = validatePackageJsonDependency packageJson spec Optional

validatePackageJsonDependency :: P.PackageJson -> PackageSpecification -> PackageRequirement -> [ErrorMsg]
validatePackageJsonDependency packageJson (packageName, expectedPackageVersion) constraint =
  case getPackageJsonDependency of
    Just actualPackageVersion ->
      if actualPackageVersion == expectedPackageVersion
        then []
        else [incorrectPackageVersionErrorMessage]
    _notListed ->
      if isInWrongDependencyType
        then [wrongDependencyTypeErrorMessage]
        else case constraint of
          RequiredRuntime -> [missingRequiredPackageErrorMessage]
          RequiredDevelopment -> [missingRequiredPackageErrorMessage]
          Optional -> []
  where
    getPackageJsonDependency :: Maybe P.PackageVersion
    getPackageJsonDependency = case constraint of
      RequiredRuntime -> M.lookup packageName $ P.dependencies packageJson
      RequiredDevelopment -> M.lookup packageName $ P.devDependencies packageJson
      Optional ->
        M.lookup packageName (P.dependencies packageJson)
          <|> M.lookup packageName (P.devDependencies packageJson)

    isInWrongDependencyType :: Bool
    isInWrongDependencyType = case constraint of
      RequiredRuntime -> M.member packageName (P.devDependencies packageJson)
      RequiredDevelopment -> M.member packageName (P.dependencies packageJson)
      Optional -> False

    packageJsonDependencyKey :: String
    packageJsonDependencyKey = case constraint of
      RequiredRuntime -> "dependencies"
      RequiredDevelopment -> "devDependencies"
      Optional -> "dependencies or devDependencies"

    oppositePackageJsonDependencyKey :: String -> String
    oppositePackageJsonDependencyKey "dependencies" = "devDependencies"
    oppositePackageJsonDependencyKey "devDependencies" = "dependencies"
    oppositePackageJsonDependencyKey _ = error "Unknown dependency key"

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

    wrongDependencyTypeErrorMessage :: ErrorMsg
    wrongDependencyTypeErrorMessage =
      unwords
        [ "Wasp requires package",
          show packageName,
          "to be in",
          packageJsonDependencyKey,
          "instead of",
          oppositePackageJsonDependencyKey packageJsonDependencyKey,
          "in package.json."
        ]
