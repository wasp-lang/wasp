module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Map as M
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.DepVersions (prismaVersion, typescriptVersion)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.SdkGenerator.DepVersions (tailwindCssVersion)
import Wasp.Generator.ServerGenerator.Common (expressTypesVersion)
import qualified Wasp.Generator.TailwindConfigFile as TCF
import Wasp.Generator.WebAppGenerator.DepVersions (reactRouterVersion, reactTypesVersion, reactVersion, viteVersion)

data PackageRequirement
  = RequiredRuntime
  | RequiredDevelopment
  | Optional

type PackageSpecification = (P.PackageName, P.PackageVersion)

validatePackageJson :: AppSpec -> [GeneratorError]
validatePackageJson spec =
  validateRuntimeDependencies spec
    ++ validateDevelopmentDependencies spec
    ++ validateOptionalDependencies spec

validateRuntimeDependencies :: AppSpec -> [GeneratorError]
validateRuntimeDependencies spec =
  concat
    [ validateRuntime ("wasp", "file:.wasp/out/sdk/wasp"),
      validateRuntime ("react-router-dom", show reactRouterVersion),
      -- Installing the wrong version of "react-router-dom" can make users believe that they
      -- can use features that are not available in the version that Wasp supports.
      validateRuntime ("react", show reactVersion),
      validateRuntime ("react-dom", show reactVersion)
    ]
  where
    validateRuntime packageSpec = validatePackageJsonDependency packageJson packageSpec RequiredRuntime
    packageJson = AS.packageJson spec

validateDevelopmentDependencies :: AppSpec -> [GeneratorError]
validateDevelopmentDependencies spec =
  concat $
    [ validateDevelopment ("vite", show viteVersion),
      validateDevelopment ("prisma", show prismaVersion)
    ]
      ++ tailwindValidations
  where
    validateDevelopment packageSpec = validatePackageJsonDependency packageJson packageSpec RequiredDevelopment

    tailwindValidations =
      [ validateDevelopment ("tailwindcss", show tailwindCssVersion) | TCF.isTailwindUsed spec
      ]

    packageJson = AS.packageJson spec

validateOptionalDependencies :: AppSpec -> [GeneratorError]
validateOptionalDependencies spec =
  concat
    [ validateOptional ("typescript", show typescriptVersion),
      validateOptional ("@types/react", show reactTypesVersion),
      validateOptional ("@types/express", show expressTypesVersion)
    ]
  where
    validateOptional packageSpec = validatePackageJsonDependency packageJson packageSpec Optional

    packageJson = AS.packageJson spec

validatePackageJsonDependency :: P.PackageJson -> PackageSpecification -> PackageRequirement -> [GeneratorError]
validatePackageJsonDependency packageJson (packageName, expectedPackageVersion) requirement =
  case maybePackageJsonDepedency of
    Just actualPackageVersion ->
      if actualPackageVersion == expectedPackageVersion
        then []
        else [incorrectPackageVersionErrorMessage]
    Nothing ->
      if isInWrongLocation requirement
        then [wrongDependencyTypeErrorMessage]
        else getMissingPackageError requirement
  where
    maybePackageJsonDepedency :: Maybe P.PackageVersion
    maybePackageJsonDepedency = case requirement of
      RequiredRuntime -> M.lookup packageName $ P.dependencies packageJson
      RequiredDevelopment -> M.lookup packageName $ P.devDependencies packageJson
      Optional ->
        M.lookup packageName (P.dependencies packageJson)
          <|> M.lookup packageName (P.devDependencies packageJson)

    isInWrongLocation :: PackageRequirement -> Bool
    isInWrongLocation = \case
      RequiredRuntime -> M.member packageName (P.devDependencies packageJson)
      RequiredDevelopment -> M.member packageName (P.dependencies packageJson)
      Optional -> False

    getMissingPackageError :: PackageRequirement -> [GeneratorError]
    getMissingPackageError = \case
      RequiredRuntime -> [missingRequiredPackageErrorMessage]
      RequiredDevelopment -> [missingRequiredPackageErrorMessage]
      Optional -> []

    incorrectPackageVersionErrorMessage :: GeneratorError
    incorrectPackageVersionErrorMessage =
      GenericGeneratorError $
        unwords
          [ "Wasp requires package",
            show packageName,
            "to be version",
            show expectedPackageVersion,
            "in package.json."
          ]

    missingRequiredPackageErrorMessage :: GeneratorError
    missingRequiredPackageErrorMessage =
      GenericGeneratorError $
        unwords
          [ "Wasp requires package",
            show packageName,
            "with version",
            show expectedPackageVersion,
            "to be in",
            show $ getExpectedPackageJsonDependencyKey requirement,
            "in package.json."
          ]

    wrongDependencyTypeErrorMessage :: GeneratorError
    wrongDependencyTypeErrorMessage =
      GenericGeneratorError $
        unwords $
          [ "Wasp requires package",
            show packageName,
            "to be in",
            show $ getExpectedPackageJsonDependencyKey requirement
          ]
            ++ ( case getOppositePackageJsonDepedencyKey requirement of
                   Just oppositeKey -> ["and not in", show oppositeKey]
                   Nothing -> []
               )
            ++ ["in package.json."]

getExpectedPackageJsonDependencyKey :: PackageRequirement -> String
getExpectedPackageJsonDependencyKey = \case
  RequiredRuntime -> "dependencies"
  RequiredDevelopment -> "devDependencies"
  Optional -> "dependencies or devDependencies"

getOppositePackageJsonDepedencyKey :: PackageRequirement -> Maybe String
getOppositePackageJsonDepedencyKey = \case
  RequiredRuntime -> Just "devDependencies"
  RequiredDevelopment -> Just "dependencies"
  Optional -> Nothing
