module Wasp.Project.ExternalConfig
  ( analyzeExternalConfigs,
    findPackageJsonFile,
    ExternalConfigs (..),
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import qualified Wasp.ExternalConfig.PackageJson as P
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.ExternalConfig.PackageJson (validatePackageJson)
import Wasp.Generator.ExternalConfig.TsConfig (validateTsConfig)
import Wasp.Project.Common
  ( CompileError,
    PackageJsonFile,
    TsConfigFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    packageJsonInWaspProjectDir,
    tsConfigInWaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

data ExternalConfigs = ExternalConfigs
  { _packageJson :: P.PackageJson,
    _tsConfig :: T.TsConfig
  }
  deriving (Show)

analyzeExternalConfigs :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] ExternalConfigs)
analyzeExternalConfigs waspDir = runExceptT $ do
  packageJsonContent <- ExceptT $ analyzePackageJsonContent waspDir
  tsConfigContent <- ExceptT $ analyzeTsConfigContent waspDir

  return $
    ExternalConfigs
      { _packageJson = packageJsonContent,
        _tsConfig = tsConfigContent
      }

analyzePackageJsonContent :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] P.PackageJson)
analyzePackageJsonContent waspProjectDir = runExceptT $ do
  packageJsonFile <- ExceptT findPackageJsonFileOrError
  packageJson <- ExceptT $ readPackageJsonFile packageJsonFile
  ExceptT $ validatePackageJson packageJson
  where
    findPackageJsonFileOrError = maybeToEither [fileNotFoundMessage] <$> findPackageJsonFile waspProjectDir
    fileNotFoundMessage = "Couldn't find the package.json file in the " ++ toFilePath waspProjectDir ++ " directory"

findPackageJsonFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File PackageJsonFile)))
findPackageJsonFile waspProjectDir = findFileInWaspProjectDir waspProjectDir packageJsonInWaspProjectDir

readPackageJsonFile :: Path' Abs (File PackageJsonFile) -> IO (Either [CompileError] P.PackageJson)
readPackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither ["Error parsing the package.json file"] $ Aeson.decode byteString

analyzeTsConfigContent :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] T.TsConfig)
analyzeTsConfigContent waspDir = runExceptT $ do
  tsConfigFile <- ExceptT findTsConfigOrError
  tsConfig <- ExceptT $ readTsConfigFile tsConfigFile
  ExceptT $ validateTsConfig tsConfig
  where
    findTsConfigOrError = maybeToEither [fileNotFoundMessage] <$> findTsConfigFile waspDir
    fileNotFoundMessage = "Couldn't find the tsconfig.json file in the " ++ toFilePath waspDir ++ " directory"

findTsConfigFile :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (File TsConfigFile)))
findTsConfigFile waspProjectDir = findFileInWaspProjectDir waspProjectDir tsConfigInWaspProjectDir

readTsConfigFile :: Path' Abs (File TsConfigFile) -> IO (Either [CompileError] T.TsConfig)
readTsConfigFile tsConfigFile = do
  tsConfigContent <- IOUtil.readFileBytes tsConfigFile

  parseResult <- parseJsonWithComments . BS.toString $ tsConfigContent

  case parseResult of
    Right tsConfig -> return $ Right tsConfig
    Left err -> return $ Left ["Failed to parse tsconfig.json file: " ++ err]
