module Wasp.Project.ExternalConfig.TsConfig
  ( parseAndValidateTsConfigFile,
  )
where

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import StrongPath (Abs, Dir, File, Path', Rel, fromRelFile, toFilePath)
import Validation (Validation (..), eitherToValidation)
import Wasp.ExternalConfig.TsConfig (TsConfigFile, parseTsConfigFile)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.Project.Common (CompileError, WaspProjectDir, findFileInWaspProjectDir)

parseAndValidateTsConfigFile ::
  (TsConfigFile f) =>
  (T.TsConfig -> [CompileError]) ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) (File f) ->
  IO (Validation [CompileError] T.TsConfig)
parseAndValidateTsConfigFile validateTsConfig waspDir someTsConfigInProjectDir =
  fmap eitherToValidation . runExceptT $ do
    tsConfigFile <- withExceptT (: []) $ ExceptT tsConfigOrError
    tsConfigContents <- withExceptT (: []) $ ExceptT $ parseTsConfigFile tsConfigFile
    case validateTsConfig tsConfigContents of
      [] -> return tsConfigContents
      errors -> liftEither $ Left errors
  where
    tsConfigOrError = maybeToEither fileNotFoundMessage <$> findFileInWaspProjectDir waspDir someTsConfigInProjectDir
    fileNotFoundMessage = "Couldn't find " ++ fromRelFile someTsConfigInProjectDir ++ " in the " ++ toFilePath waspDir ++ " directory"
