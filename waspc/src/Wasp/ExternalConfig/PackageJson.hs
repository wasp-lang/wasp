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
      validatePackageInDeps (dependencies packageJson) ("wasp", "file:.wasp/out/sdk/wasp")

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs File'))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir

readPackageJsonFile :: Path' Abs File' -> IO (Either [CompileError] PackageJson)
readPackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither ["Error parsing the package.json file"] $ Aeson.decode byteString

validatePackageInDeps :: DependenciesMap -> (PackageName, PackageVersion) -> [CompileError]
validatePackageInDeps deps (packageName, expectedPackageVersion) =
  case M.lookup packageName deps of
    Just actualPackageVersion ->
      if actualPackageVersion == expectedPackageVersion
        then []
        else [packageVersionMismatchMessage]
    Nothing -> [packageNotFoundMessage]
  where
    packageVersionMismatchMessage = "The package \"" ++ packageName ++ "\" is not the expected version \"" ++ expectedPackageVersion ++ "\"."
    packageNotFoundMessage = "The package \"" ++ packageName ++ "\" is not found in the dependencies."

getDependencies :: PackageJson -> [Dependency]
getDependencies packageJson = D.fromList $ M.toList $ dependencies packageJson

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies packageJson = D.fromList $ M.toList $ devDependencies packageJson
