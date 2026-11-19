module Wasp.Project.ExternalConfig.TsConfig
  ( parseAndValidateTsConfigFile,
    extendsImportAliasesValidator,
    noCustomPathsValidator,
    -- Exported only for testing
    validateTsConfig,
  )
where

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, withExceptT)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (isJust)
import StrongPath (Abs, Dir, File, Path', Rel, fromRelFile, toFilePath)
import Validation (Validation (..), eitherToValidation)
import Wasp.ExternalConfig.TsConfig (TsConfigFile, parseTsConfigFile)
import qualified Wasp.ExternalConfig.TsConfig as T
import Wasp.NpmPackageNames (waspNpmPackageName)
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

-- | \@wasp/wasp comes with a tsconfig whose `paths` map the `wasp/*` imports
-- to the generated code and to the Wasp Spec. Neither is a package in
-- `node_modules`, so TypeScript can't find them on its own.
extendsImportAliasesValidator :: V.Validator T.TsConfig
extendsImportAliasesValidator =
  V.inField ("extends", fmap T.extendedConfigPaths . T.extends)
    $ V.requiredWith ("Must extend " ++ show importAliasesTsConfigPath ++ ", so TypeScript can find the `wasp/*` imports.")
    $ V.containsAll [importAliasesTsConfigPath]
  where
    importAliasesTsConfigPath = waspNpmPackageName ++ "/tsconfig.paths.json"

-- | TypeScript doesn't merge `paths` with the extended config's, it replaces them.
noCustomPathsValidator :: V.Validator T.CompilerOptions
noCustomPathsValidator = V.inField ("paths", T.paths) $ \paths ->
  if isJust paths
    then V.failure "Wasp doesn't support custom `paths`, because they would replace the ones that map the `wasp/*` imports."
    else V.success

validateTsConfig :: V.Validator T.TsConfig -> String -> T.TsConfig -> [CompileError]
validateTsConfig validator tsConfigFileName tsConfigContents =
  show <$> V.execValidator (V.withFileName tsConfigFileName validator) tsConfigContents
