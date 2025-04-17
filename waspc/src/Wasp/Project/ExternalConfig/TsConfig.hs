module Wasp.Project.ExternalConfig.TsConfig
  ( readSrcTsConfigFile,
  )
where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', Rel, basename, fromRelFile, toFilePath)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common
  ( SrcTsConfigFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
  )
import Wasp.Util (indent)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

readSrcTsConfigFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either String T.TsConfig)
readSrcTsConfigFile waspDir srcTsConfigPath = runExceptT $ do
  tsConfigFileContents <- ExceptT findTsConfigOrError
  ExceptT $ readTsConfigFile tsConfigFileContents
  where
    findTsConfigOrError = maybeToEither fileNotFoundMessage <$> findFileInWaspProjectDir waspDir srcTsConfigPath
    fileNotFoundMessage = "Couldn't find the tsconfig.json file in the " ++ toFilePath waspDir ++ " directory"

readTsConfigFile :: Path' Abs (File SrcTsConfigFile) -> IO (Either String T.TsConfig)
readTsConfigFile tsConfigFile = do
  tsConfigContent <- IOUtil.readFileBytes tsConfigFile
  parseResult <- parseJsonWithComments . BS.toString $ tsConfigContent
  return $ left ((errorMessagePrefix ++) . indent 2) parseResult
  where
    errorMessagePrefix = "Failed to parse '" ++ baseTsConfigFilePath ++ "':\n"
    baseTsConfigFilePath = fromRelFile (basename tsConfigFile)
