module Wasp.Project.ExternalConfig.PackageJson
  ( parseAndValidateUserPackageJson,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Validation (Validation (..))
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import Wasp.Project.Common
  ( CompileError,
    UserPackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
  )

parseAndValidateUserPackageJson :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] PackageJson)
parseAndValidateUserPackageJson waspDir =
  readUserPackageJsonFile waspDir >>= \case
    Left err -> return $ Failure [err]
    Right packageJson -> return $ Success packageJson

readUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either String PackageJson)
readUserPackageJsonFile waspDir = runExceptT $ do
  packageJsonFile <- ExceptT findUserPackageJsonFileOrError
  ExceptT $ parsePackageJsonFile packageJsonFile
  where
    findUserPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findUserPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findUserPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir
