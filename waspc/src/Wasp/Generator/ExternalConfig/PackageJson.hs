module Wasp.Generator.ExternalConfig.PackageJson
  ( validatePackageJson,
  )
where

import qualified Data.Map as M
import qualified Wasp.ExternalConfig.PackageJson as P
import Wasp.Generator.Common (prismaVersion)
import Wasp.Generator.ExternalConfig.Common (ErrorMsg)
import Wasp.Generator.WebAppGenerator.Common (reactRouterVersion)

validatePackageJson :: P.PackageJson -> [ErrorMsg]
validatePackageJson packageJson =
  concat
    [ validate ("wasp", "file:.wasp/out/sdk/wasp") IsListedWithExactVersion,
      validate ("prisma", show prismaVersion) IsListedAsDevWithExactVersion,
      -- Installing the wrong version of "react-router-dom" can make users believe that they
      -- can use features that are not available in the version that Wasp supports.
      validate ("react-router-dom", show reactRouterVersion) HasExactVersionIfListed
    ]
  where
    validate = validateDep packageJson

data PackageValidationType = IsListedWithExactVersion | IsListedAsDevWithExactVersion | HasExactVersionIfListed

validateDep :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> PackageValidationType -> [String]
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

    incorrectVersionMessage :: String
    incorrectVersionMessage =
      unwords
        ["Wasp requires package", show packageName, "to be version", show expectedPackageVersion, "in package.json."]

    requiredPackageMessage :: String -> String
    requiredPackageMessage packageJsonLocation =
      unwords
        [ "Wasp requires package",
          show packageName,
          "with version",
          show expectedPackageVersion,
          "in",
          show packageJsonLocation,
          "in package.json."
        ]
