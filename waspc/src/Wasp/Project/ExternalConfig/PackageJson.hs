module Wasp.Project.ExternalConfig.PackageJson
  ( analyzePackageJsonFile,
    findPackageJsonFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import qualified Data.Aeson as Aeson
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.Generator.ExternalConfig.PackageJson (validatePackageJson)
import Wasp.Project.Common
  ( PackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil

analyzePackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either [String] PackageJson)
analyzePackageJsonFile waspProjectDir = runExceptT $ do
  packageJsonFile <- ExceptT findPackageJsonFileOrError
  packageJson <- ExceptT $ readPackageJsonFile packageJsonFile
  case validatePackageJson packageJson of
    [] -> return packageJson
    errors -> throwError errors
  where
    findPackageJsonFileOrError = maybeToEither [fileNotFoundMessage] <$> findPackageJsonFile waspProjectDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspProjectDir ++ " directory"

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File PackageJsonFile)))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir

readPackageJsonFile :: Path' Abs (File PackageJsonFile) -> IO (Either [String] PackageJson)
readPackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither ["Error parsing the package.json file"] $ Aeson.decode byteString
