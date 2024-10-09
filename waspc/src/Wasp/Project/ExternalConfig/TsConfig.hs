module Wasp.Project.ExternalConfig.TsConfig
  ( analyzeSrcTsConfigFile,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', Rel, toFilePath)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.ExternalConfig.TsConfig (validateSrcTsConfig)
import Wasp.Project.Common
  ( SrcTsConfigFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
  )
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

analyzeSrcTsConfigFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [String] T.TsConfig)
analyzeSrcTsConfigFile waspDir srcTsConfigFile = runExceptT $ do
  tsConfigFileContents <- ExceptT findTsConfigOrError
  srcTsConfigContents <- ExceptT $ readSrcTsConfigFile tsConfigFileContents
  case validateSrcTsConfig srcTsConfigContents of
    [] -> return srcTsConfigContents
    errors -> throwError errors
  where
    findTsConfigOrError = maybeToEither [fileNotFoundMessage] <$> findFileInWaspProjectDir waspDir srcTsConfigFile
    fileNotFoundMessage = "Couldn't find the tsconfig.json file in the " ++ toFilePath waspDir ++ " directory"

readSrcTsConfigFile :: Path' Abs (File SrcTsConfigFile) -> IO (Either [String] T.TsConfig)
readSrcTsConfigFile tsConfigFile = do
  tsConfigContent <- IOUtil.readFileBytes tsConfigFile

  parseResult <- parseJsonWithComments . BS.toString $ tsConfigContent

  case parseResult of
    Right tsConfig -> return $ Right tsConfig
    Left err -> return $ Left ["Failed to parse tsconfig.json file: " ++ err]
