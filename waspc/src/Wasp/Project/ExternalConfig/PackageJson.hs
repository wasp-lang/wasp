module Wasp.Project.ExternalConfig.PackageJson
  ( parseAndValidateUserPackageJson,
    findUserPackageJsonFile,

    -- * Exported for testing only
    packageJsonValidator,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import qualified Data.Map as M
import Data.Maybe (isJust)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Validation (Validation, eitherToValidation)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import qualified Wasp.ExternalConfig.Npm.PackageJson as PackageJson
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common
  ( CompileError,
    TsConfigPaths (..),
    UserPackageJsonFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    userPackageJsonInWaspProjectDir,
  )
import qualified Wasp.Validator as WaspV

parseAndValidateUserPackageJson :: Path' Abs (Dir WaspProjectDir) -> TsConfigPaths -> IO (Validation [CompileError] PackageJson)
parseAndValidateUserPackageJson waspDir tsConfigPaths = fmap eitherToValidation . runExceptT $ do
  packageJsonFile <- withExceptT (: []) $ ExceptT userPackageJsonFileOrError
  packageJson <- withExceptT (: []) $ ExceptT $ parsePackageJsonFile packageJsonFile
  case validatePackageJson tsConfigPaths packageJson of
    [] -> return packageJson
    errors -> liftEither $ Left errors
  where
    userPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findUserPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findUserPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir userPackageJsonInWaspProjectDir

packageJsonValidator :: TsConfigPaths -> WaspV.Validator PackageJson
packageJsonValidator tsConfigPaths
  | isWaspTsProject tsConfigPaths = requiredNodeTypesValidator
  | otherwise = const WaspV.success
  where
    isWaspTsProject = isJust . waspTsConfig

    requiredNodeTypesValidator :: WaspV.Validator PackageJson
    requiredNodeTypesValidator =
      WaspV.withFileName "package.json" $
        nodeTypesNotInDependenciesValidator
          `WaspV.and` nodeTypesInDevDependenciesValidator

    nodeTypesNotInDependenciesValidator :: WaspV.Validator PackageJson
    nodeTypesNotInDependenciesValidator =
      WaspV.inField ("dependencies", PackageJson.dependencies) $
        WaspV.inField (nodeTypesPackageName, M.lookup nodeTypesPackageName) nodeTypesMissingValidator

    nodeTypesInDevDependenciesValidator :: WaspV.Validator PackageJson
    nodeTypesInDevDependenciesValidator =
      WaspV.inField ("devDependencies", PackageJson.devDependencies) $
        WaspV.inField (nodeTypesPackageName, M.lookup nodeTypesPackageName) $
          WaspV.eqJust requiredNodeTypesVersion

    nodeTypesMissingValidator :: WaspV.Validator (Maybe PackageJson.PackageVersion)
    nodeTypesMissingValidator Nothing = WaspV.success
    nodeTypesMissingValidator _ = WaspV.failure $ "Wasp requires package " ++ show nodeTypesPackageName ++ " to be in \"devDependencies\"."

nodeTypesPackageName :: PackageJson.PackageName
nodeTypesPackageName = "@types/node"

requiredNodeTypesVersion :: PackageJson.PackageVersion
requiredNodeTypesVersion = NodeVersion.nodeTypesPackageVersionRange NodeVersion.oldestWaspSupportedNodeVersion

validatePackageJson :: TsConfigPaths -> PackageJson -> [CompileError]
validatePackageJson tsConfigPaths packageJson =
  show <$> WaspV.execValidator (packageJsonValidator tsConfigPaths) packageJson
