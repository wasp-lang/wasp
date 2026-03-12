module Wasp.Project.ExternalConfig.PackageJson
  ( readUserPackageJsonFile,
    findPackageJsonFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import Wasp.Project.Common
  ( UserPackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
  )

readUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String PackageJson)
readUserPackageJsonFile waspDir = runExceptT $ do
  packageJsonFile <- ExceptT findPackageJsonFileOrError
  ExceptT $ parsePackageJsonFile packageJsonFile
  where
    findPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir
