module Wasp.Project.ExternalConfig.TsConfig
  ( analyzeSrcTsConfigFile,
  )
where

import Control.Arrow (left)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', Rel, basename, fromRelFile, toFilePath)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Generator.ExternalConfig.TsConfig (validateSrcTsConfig)
import Wasp.Project.Common
  ( SrcTsConfigFile,
    WaspProjectDir,
    findFileInWaspProjectDir,
  )
import Wasp.Util (indent)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (parseJsonWithComments)

analyzeSrcTsConfigFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File SrcTsConfigFile) ->
  IO (Either [String] T.TsConfig)
analyzeSrcTsConfigFile waspDir srcTsConfigFile = runExceptT $ do
  tsConfigFileContents <- ExceptT findTsConfigOrError
  srcTsConfigContents <- ExceptT $ left (: []) <$> readTsConfigFile tsConfigFileContents
  case validateSrcTsConfig srcTsConfigContents of
    [] -> return srcTsConfigContents
    errors -> throwError errors
  where
    findTsConfigOrError = maybeToEither [fileNotFoundMessage] <$> findFileInWaspProjectDir waspDir srcTsConfigFile
    fileNotFoundMessage = "Couldn't find the tsconfig.json file in the " ++ toFilePath waspDir ++ " directory"

-- TODO: Reduce polymorphism, should only work with TsConfig files
readTsConfigFile :: Path' Abs (File f) -> IO (Either String T.TsConfig)
readTsConfigFile tsConfigFile = do
  tsConfigContent <- IOUtil.readFileBytes tsConfigFile
  parseResult <- parseJsonWithComments . BS.toString $ tsConfigContent
  return $ left ((errorMessagePrefix ++) . indent 2) parseResult
  where
    errorMessagePrefix = "Failed to parse '" ++ baseTsConfigFilePath ++ "':\n"
    baseTsConfigFilePath = fromRelFile (basename tsConfigFile)
