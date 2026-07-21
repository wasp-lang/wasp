module Wasp.Project.ExternalConfig.PackageJson
  ( parseAndValidateUserPackageJson,
    parseAndValidateModulePackageJson,
    findUserPackageJsonFile,
    validatePackageJsonForProject,
    validatePackageJsonForModule,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), liftEither, runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (isJust)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import Validation (Validation, eitherToValidation)
import Wasp.ExternalConfig.Npm.PackageJson (PackageJson, parsePackageJsonFile)
import qualified Wasp.ExternalConfig.Npm.PackageJson as PackageJson
import Wasp.ExternalConfig.Npm.PackageJson.DepValidators
  ( DependencyType (Development),
    makeRequiredDepValidator,
  )
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
parseAndValidateUserPackageJson waspDir tsConfigPaths =
  parseAndValidatePackageJson waspDir $ validatePackageJsonForProject tsConfigPaths

parseAndValidateModulePackageJson :: Path' Abs (Dir WaspProjectDir) -> IO (Validation [CompileError] PackageJson)
parseAndValidateModulePackageJson waspDir = parseAndValidatePackageJson waspDir validatePackageJsonForModule

parseAndValidatePackageJson :: Path' Abs (Dir WaspProjectDir) -> (PackageJson -> [CompileError]) -> IO (Validation [CompileError] PackageJson)
parseAndValidatePackageJson waspDir validate = fmap eitherToValidation . runExceptT $ do
  packageJsonFile <- withExceptT (: []) $ ExceptT userPackageJsonFileOrError
  packageJson <- withExceptT (: []) $ ExceptT $ parsePackageJsonFile packageJsonFile
  case validate packageJson of
    [] -> return packageJson
    errors -> liftEither $ Left errors
  where
    userPackageJsonFileOrError = maybeToEither fileNotFoundMessage <$> findUserPackageJsonFile waspDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspDir ++ " directory"

findUserPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File UserPackageJsonFile)))
findUserPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir userPackageJsonInWaspProjectDir

validatePackageJsonForProject :: TsConfigPaths -> PackageJson -> [CompileError]
validatePackageJsonForProject tsConfigPaths packageJson =
  show <$> WaspV.execValidator (packageJsonValidator tsConfigPaths) packageJson

validatePackageJsonForModule :: PackageJson -> [CompileError]
validatePackageJsonForModule packageJson =
  show <$> WaspV.execValidator modulePackageJsonValidator packageJson

packageJsonValidator :: TsConfigPaths -> WaspV.Validator PackageJson
packageJsonValidator tsConfigPaths =
  WaspV.withFileName "package.json" $
    WaspV.all [waspTsPackageJsonValidator tsConfigPaths]

modulePackageJsonValidator :: WaspV.Validator PackageJson
modulePackageJsonValidator =
  WaspV.withFileName "package.json" $
    WaspV.all
      [ WaspV.inField ("name", PackageJson.name) nonEmptyPackageNameValidator,
        WaspV.inField ("wasp", PackageJson.wasp) $
          WaspV.required $
            WaspV.inField ("module", PackageJson.module_) $
              WaspV.required (const WaspV.success)
      ]

nonEmptyPackageNameValidator :: WaspV.Validator String
nonEmptyPackageNameValidator packageName
  | null packageName = WaspV.failure "Must not be empty."
  | otherwise = WaspV.success

waspTsPackageJsonValidator :: TsConfigPaths -> WaspV.Validator PackageJson
waspTsPackageJsonValidator tsConfigPaths
  -- We require @types/node only for Wasp TS projects.
  | isWaspTsProject tsConfigPaths = nodeTypesInDevDependenciesValidator
  | otherwise = const WaspV.success
  where
    isWaspTsProject = isJust . waspTsConfig

nodeTypesInDevDependenciesValidator :: WaspV.Validator PackageJson
nodeTypesInDevDependenciesValidator = makeRequiredDepValidator Development ("@types/node", requiredNodeTypesVersion)
  where
    requiredNodeTypesVersion = show $ NodeVersion.nodeTypesVersionRangeMatchingNodeMajor NodeVersion.oldestWaspSupportedNodeVersion
