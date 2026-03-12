module Wasp.Project.ExternalConfig.PackageJson
  ( parseAndValidateUserPackageJson,
    findUserPackageJsonFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Bifunctor (first)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Validation (Validation (..))
import qualified Validation as V
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import Wasp.Project.Common
  ( CompileError,
    UserPackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    userPackageJsonInWaspProjectDir,
  )

parseAndValidateUserPackageJson :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] PackageJson)
parseAndValidateUserPackageJson waspDir = fmap (V.eitherToValidation . first (: [])) . runExceptT $ do
  packageJsonFile <- ExceptT userPackageJsonFileOrError
  ExceptT $ parsePackageJsonFile packageJsonFile
  where
    userPackageJsonFileOrError =
      maybeToEither fileNotFoundMessage <$> findUserPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findUserPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir userPackageJsonInWaspProjectDir
