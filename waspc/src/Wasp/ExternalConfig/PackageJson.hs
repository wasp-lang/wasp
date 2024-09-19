{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.PackageJson
  ( PackageJson (..),
    getDependencies,
    getDevDependencies,
    analyzePackageJsonContent,
    findPackageJsonFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics (Generic)
import StrongPath (Abs, Dir, File', Path', toFilePath)
import Wasp.AppSpec.App.Dependency (Dependency)
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.Common
  ( prismaVersion,
    reactRouterVersion,
  )
import Wasp.Project.Common
  ( CompileError,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
  )
import Wasp.Util (maybeToEither)
import qualified Wasp.Util.IO as IOUtil

data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !DependenciesMap,
    devDependencies :: !DependenciesMap
  }
  deriving (Show, Generic)

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String

instance FromJSON PackageJson

analyzePackageJsonContent :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] PackageJson)
analyzePackageJsonContent waspProjectDir = runExceptT $ do
  packageJsonFile <- ExceptT findPackageJsonFileOrError
  packageJson <- ExceptT $ readPackageJsonFile packageJsonFile
  ExceptT $ validatePackageJson packageJson
  where
    findPackageJsonFileOrError = maybeToEither [fileNotFoundMessage] <$> findPackageJsonFile waspProjectDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspProjectDir ++ " directory"

validatePackageJson :: PackageJson -> IO (Either [CompileError] PackageJson)
validatePackageJson packageJson =
  return $
    if null packageJsonErrors
      then Right packageJson
      else Left packageJsonErrors
  where
    packageJsonErrors =
      concat
        [ -- Wasp needs the Wasp SDK to be installed in the project.
          validate ("wasp", "file:.wasp/out/sdk/wasp", RequiredPackage),
          -- Wrong version of Prisma will break the generated code.
          validate ("prisma", show prismaVersion, RequiredDevPackage),
          -- Installing the wrong version of "react-router-dom" can make users believe that they
          -- can use features that are not available in the version that Wasp supports.
          validate ("react-router-dom", show reactRouterVersion, OptionalPackage)
        ]
    validate = validatePackageInDeps packageJson

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir

readPackageJsonFile :: Path' Abs File' -> IO (Either [CompileError] PackageJson)
readPackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither ["Error parsing the package.json file"] $ Aeson.decode byteString

data PackageValidationType = RequiredPackage | RequiredDevPackage | OptionalPackage

validatePackageInDeps :: PackageJson -> (PackageName, PackageVersion, PackageValidationType) -> [CompileError]
validatePackageInDeps packageJson (packageName, expectedPackageVersion, validationType) =
  case map (M.lookup packageName) depsToCheck of
    (Just actualPackageVersion : _) ->
      if actualPackageVersion == expectedPackageVersion
        then []
        else [incorrectVersionMessage]
    _rest -> case validationType of
      RequiredPackage -> [requiredPackageMessage "dependencies"]
      RequiredDevPackage -> [requiredPackageMessage "devDependencies"]
      OptionalPackage -> []
  where
    depsToCheck = case validationType of
      RequiredPackage -> [dependencies packageJson]
      RequiredDevPackage -> [devDependencies packageJson]
      -- Users can install packages that don't need to be strictly in dependencies or devDependencies
      -- which means Wasp needs to check both to validate the correct version of the package.
      OptionalPackage -> [dependencies packageJson, devDependencies packageJson]

    incorrectVersionMessage :: String
    incorrectVersionMessage =
      unwords
        ["The", show packageName, "package must have version", show expectedPackageVersion, "in package.json."]

    requiredPackageMessage :: String -> String
    requiredPackageMessage packageJsonLocation =
      unwords
        [ "The",
          show packageName,
          "package with version",
          show expectedPackageVersion,
          "must be defined in",
          show packageJsonLocation,
          "in package.json."
        ]
