module Wasp.Project.ExternalConfig.TsConfig
  ( analyzeTsConfigFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', toFilePath)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.ExternalConfig.TsConfig (validateTsConfig)
import Wasp.Project.Common
  ( CompileError,
    TsConfigFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
    tsConfigInWaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

analyzeTsConfigFile :: Path' Abs (Dir WaspProjectDir) -> IO (Either [CompileError] T.TsConfig)
analyzeTsConfigFile waspDir = runExceptT $ do
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
