module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import Control.Selective (bindS)
import qualified Data.Map as M
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
    validateAll_,
  )
import qualified Wasp.Generator.WebAppGenerator.DepVersions as D

validatePackageJson :: P.PackageJson -> [GeneratorError]
validatePackageJson =
  (fmap (GenericGeneratorError . show) <$>) . execValidator $ do
    fileValidator "package.json" validateDependencies
  where
    validateDependencies =
      validateAll_ $
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
validateOptionalDependency dep@(pkgName, pkgVersion) =
  validateAll_
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
  Validator P.PackageJson ()
validateRequiredDependency reqType dep@(pkgName, pkgVersion) pkgJson =
  -- `bindS` is the synonym of `>>=` for the `Selective` applicatives, that is,
  -- an effectul computation that depends on the previous one's result.
  --
  -- We use it here so that if we show the first error about the dependency
  -- being in the wrong place, it will short-circuit and not show the second
  -- error about it missing in in its correct place.
  bindS
    (forWrongDependency validationForWrongPlace pkgJson)
    (const $ forCorrectDependency validationForCorrectPlace pkgJson)
  where
    validationForWrongPlace NotPresent = pure ()
    validationForWrongPlace _ = wrongDepTypeError

    validationForCorrectPlace CorrectVersion = pure ()
    validationForCorrectPlace NotPresent = missingPackageError
    validationForCorrectPlace IncorrectVersion = incorrectPackageVersionError

    forCorrectDependency = forDependency reqType dep
    forWrongDependency = forDependency (oppositeDepType reqType) dep

    oppositeDepType Runtime = Development
    oppositeDepType Development = Runtime

    wrongDepTypeError =
      failure $
        unwords
          [ "Wasp requires package",
            show pkgName,
            "to be in",
            show (reqTypeFieldName reqType) ++ "."
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

data RequirementType = Runtime | Development

forDependency ::
  RequirementType ->
  PackageSpecification ->
  (DependencyCheckResult -> Validation a) ->
  Validator P.PackageJson a
forDependency reqType (pkgName, pkgVersion) f =
  field (reqTypeFieldName reqType) (reqTypeGetter reqType) $
    field pkgName (M.lookup pkgName) (f . checkDependency pkgVersion)
  where
    reqTypeGetter Runtime = P.dependencies
    reqTypeGetter Development = P.devDependencies

reqTypeFieldName :: RequirementType -> String
reqTypeFieldName Runtime = "dependencies"
reqTypeFieldName Development = "devDependencies"

data DependencyCheckResult = CorrectVersion | IncorrectVersion | NotPresent
  deriving (Eq, Enum, Bounded)

checkDependency :: P.PackageVersion -> Maybe P.PackageVersion -> DependencyCheckResult
checkDependency expectedVersion (Just actualVersion)
  | expectedVersion == actualVersion = CorrectVersion
  | otherwise = IncorrectVersion
checkDependency _ Nothing = NotPresent
