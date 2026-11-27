module Wasp.Project.ExternalConfig.PackageJson
  ( parseUserPackageJson,
    findUserPackageJsonFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Validation (Validation, eitherToValidation)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import Wasp.Project.Common
  ( CompileError,
    UserPackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    userPackageJsonInWaspProjectDir,
  )

parseUserPackageJson :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] PackageJson)
parseUserPackageJson waspDir = fmap eitherToValidation . runExceptT $ do
  packageJsonFile <- withExceptT (: []) $ ExceptT userPackageJsonFileOrError
  withExceptT (: []) $ ExceptT $ parsePackageJsonFile packageJsonFile
  where
    userPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findUserPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findUserPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir userPackageJsonInWaspProjectDir
