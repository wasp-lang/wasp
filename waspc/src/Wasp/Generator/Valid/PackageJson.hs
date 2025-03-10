module Wasp.Generator.Valid.PackageJson
  ( validatePackageJson,
  )
where

import qualified Data.Map as M
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.PackageJson as P
import Wasp.Generator.DepVersions (prismaVersion)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Generator.SdkGenerator.DepVersions (tailwindCssVersion)
import Wasp.Generator.WebAppGenerator.DepVersions (reactRouterVersion, reactVersion)

validatePackageJson :: AppSpec -> [GeneratorError]
validatePackageJson spec =
  concat
    ( [ validate ("wasp", "file:.wasp/out/sdk/wasp") IsListedWithExactVersion,
        validate ("prisma", show prismaVersion) IsListedAsDevWithExactVersion,
        -- Installing the wrong version of "react-router-dom" can make users believe that they
        -- can use features that are not available in the version that Wasp supports.
        validate ("react-router-dom", show reactRouterVersion) IsListedWithExactVersion,
        validate ("react", show reactVersion) IsListedWithExactVersion,
        validate ("react-dom", show reactVersion) IsListedWithExactVersion
      ]
        ++ tailwindValidations
    )
  where
    validate = validateDep $ AS.packageJson spec

    tailwindValidations =
      [ validate ("tailwindcss", show tailwindCssVersion) IsListedWithExactVersion | isTailwindUsed
      ]

    isTailwindUsed = AS.isTailwindUsed spec

data PackageValidationType = IsListedWithExactVersion | IsListedAsDevWithExactVersion | HasExactVersionIfListed

validateDep :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> PackageValidationType -> [GeneratorError]
validateDep packageJson (packageName, expectedPackageVersion) = \case
  IsListedWithExactVersion -> checkDeps [P.dependencies packageJson] [requiredPackageMessage "dependencies"]
  IsListedAsDevWithExactVersion -> checkDeps [P.devDependencies packageJson] [requiredPackageMessage "devDependencies"]
  HasExactVersionIfListed -> checkDeps [P.dependencies packageJson, P.devDependencies packageJson] []
  where
    checkDeps depsToCheck errorMessagesIfPackageNotListed = case map (M.lookup packageName) depsToCheck of
      (Just actualPackageVersion : _) ->
        if actualPackageVersion == expectedPackageVersion
          then []
          else [incorrectVersionMessage]
      _notListed -> errorMessagesIfPackageNotListed

    incorrectVersionMessage :: GeneratorError
    incorrectVersionMessage =
      GenericGeneratorError $
        unwords
          ["Wasp requires package", show packageName, "to be version", show expectedPackageVersion, "in package.json."]

    requiredPackageMessage :: String -> GeneratorError
    requiredPackageMessage packageJsonLocation =
      GenericGeneratorError $
        unwords
          [ "Wasp requires package",
            show packageName,
            "with version",
            show expectedPackageVersion,
            "in",
            show packageJsonLocation,
            "in package.json."
          ]
