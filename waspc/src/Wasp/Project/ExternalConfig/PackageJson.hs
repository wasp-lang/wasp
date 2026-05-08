module Wasp.Project.ExternalConfig.PackageJson
  ( parseAndValidateUserPackageJson,
    findUserPackageJsonFile,
    validatePackageJsonForProject,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import qualified Data.Map as M
import Data.Maybe (isJust)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Validation (Validation, eitherToValidation)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
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
  liftEither $ withProjectValidation packageJson
  where
    userPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findUserPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

    withProjectValidation packageJson =
      case validatePackageJsonForProject tsConfigPaths packageJson of
        [] -> Right packageJson
        errors -> Left errors

findUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findUserPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir userPackageJsonInWaspProjectDir

validatePackageJsonForProject :: TsConfigPaths -> PackageJson -> [CompileError]
validatePackageJsonForProject tsConfigPaths packageJson =
  show <$> WaspV.execValidator (packageJsonValidator tsConfigPaths) packageJson

packageJsonValidator :: TsConfigPaths -> WaspV.Validator PackageJson
packageJsonValidator tsConfigPaths =
  WaspV.withFileName "package.json" $
    WaspV.all
      [ waspTsPackageJsonValidator tsConfigPaths
      ]

-- Wasp TS specs are type-checked during analysis, before AppSpec exists, so
-- Node types have to be validated here instead of in generator validation.
waspTsPackageJsonValidator :: TsConfigPaths -> WaspV.Validator PackageJson
waspTsPackageJsonValidator tsConfigPaths
  | isWaspTsProject tsConfigPaths = nodeTypesInDevDependenciesValidator
  | otherwise = const WaspV.success
  where
    isWaspTsProject = isJust . waspTsConfig

nodeTypesInDevDependenciesValidator :: WaspV.Validator PackageJson
nodeTypesInDevDependenciesValidator =
  nodeTypesNotInDependenciesValidator
    `WaspV.and` requiredNodeTypesDevDependencyValidator

nodeTypesNotInDependenciesValidator :: WaspV.Validator PackageJson
nodeTypesNotInDependenciesValidator =
  WaspV.inField ("dependencies", P.dependencies) $
    WaspV.inField (nodeTypesPackageName, M.lookup nodeTypesPackageName) nodeTypesMissingValidator

requiredNodeTypesDevDependencyValidator :: WaspV.Validator PackageJson
requiredNodeTypesDevDependencyValidator =
  WaspV.inField ("devDependencies", P.devDependencies) $
    WaspV.inField (nodeTypesPackageName, M.lookup nodeTypesPackageName) $
      WaspV.eqJust requiredNodeTypesVersion

nodeTypesMissingValidator :: WaspV.Validator (Maybe P.PackageVersion)
nodeTypesMissingValidator Nothing = WaspV.success
nodeTypesMissingValidator _ = WaspV.failure $ "Wasp requires package " ++ show nodeTypesPackageName ++ " to be in \"devDependencies\"."

nodeTypesPackageName :: P.PackageName
nodeTypesPackageName = "@types/node"

requiredNodeTypesVersion :: P.PackageVersion
requiredNodeTypesVersion = NodeVersion.nodeTypesPackageVersionRange NodeVersion.oldestWaspSupportedNodeVersion
