module Wasp.Project.ExternalConfig.TsConfig
  ( parseAndValidateTsConfigFile,
    -- Exported only for testing
    validateTsConfig,
  )
where

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', Rel, fromRelFile, toFilePath)
import Validation (Validation (..), eitherToValidation)
import Wasp.ExternalConfig.TsConfig (TsConfigFile, parseTsConfigFile)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, WaspProjectDir, findFileInWaspProjectDir)
import qualified Wasp.Validator as V

parseAndValidateTsConfigFile ::
  (TsConfigFile f) =>
  V.Validator T.TsConfig ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File f) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateTsConfigFile validator waspDir tsConfigPathInProject =
  fmap eitherToValidation . runExceptT $ do
    absTsConfigPath <- withExceptT (: []) $ ExceptT absTsConfigPathOrError
    tsConfigContents <- withExceptT (: []) $ ExceptT $ parseTsConfigFile absTsConfigPath
    case validateTsConfig validator tsConfigFileName tsConfigContents of
      [] -> return tsConfigContents
      errors -> liftEither $ Left errors
  where
    absTsConfigPathOrError = maybeToEither fileNotFoundMessage <$> findFileInWaspProjectDir waspDir tsConfigPathInProject
    fileNotFoundMessage = "Couldn't find " ++ tsConfigFileName ++ " in the " ++ toFilePath waspDir ++ " directory"
    tsConfigFileName = fromRelFile tsConfigPathInProject

validateTsConfig :: V.Validator T.TsConfig -> String -> T.TsConfig -> [CompileError]
validateTsConfig validator tsConfigFileName tsConfigContents =
  show <$> V.execValidator (V.withFileName tsConfigFileName validator) tsConfigContents
