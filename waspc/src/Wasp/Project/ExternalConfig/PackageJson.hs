module Wasp.Project.ExternalConfig.PackageJson
  ( readPackageJsonFile,
    findPackageJsonFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.Aeson as Aeson
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson)
import Wasp.Project.Common
  ( PackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil

readPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String PackageJson)
readPackageJsonFile waspDir = runExceptT $ do
  packageJsonFile <- ExceptT findPackageJsonFileOrError
  ExceptT $ decodePackageJsonFromDisk packageJsonFile
  where
    findPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File PackageJsonFile)))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir

decodePackageJsonFromDisk :: Path' Abs (File PackageJsonFile) -> IO (Either String PackageJson)
decodePackageJsonFromDisk packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither "Error parsing the package.json file" $ Aeson.decode byteString
