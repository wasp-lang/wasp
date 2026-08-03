{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Util.EnvVarInputs
  ( EnvVarInput (..),
    SourceName,
    readEnvVarInput,
    resolveEnvVars,
    resolveEnvVarInputs,
    assertNoOverriddenEnvVars,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Set as Set
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath, showFilePathArgument)
import Wasp.Env (EnvVar, nubEnvVars, parseDotEnvFile)
import Wasp.Project.Common (WaspProjectDir, findFileInWaspProjectDir)

-- | A user-facing description of where some env vars came from, e.g. a file
-- path or a CLI option.
type SourceName = String

-- | A place the env vars given to an app can come from.
data EnvVarInput
  = -- | The environment Wasp itself was started in.
    Inherit
  | -- | A var set through a CLI option, e.g. @--server-env@.
    FromFlag SourceName EnvVar
  | -- | A dotenv file the user pointed us to through a CLI option.
    FromFileArgument FilePathArgument
  | -- | A dotenv file Wasp looks for in the project dir, e.g. @.env.server@.
    FromProjectFile (SP.Path' (SP.Rel WaspProjectDir) SP.File')

resolveEnvVarInputs :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> [EnvVar] -> [EnvVarInput] -> Command [EnvVar]
resolveEnvVarInputs projectDir forcedEnvVars envVarInputs = do
  envVarsBySource <- liftIO $ mapM (readEnvVarInput projectDir) envVarInputs
  let resolved = resolveEnvVars forcedEnvVars envVarsBySource

  case resolved of
    Left errMsg -> throwError $ CommandError "Couldn't resolve environment variables" errMsg
    Right envVars -> return envVars

readEnvVarInput :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> EnvVarInput -> IO (SourceName, [EnvVar])
readEnvVarInput _ Inherit = ("your environment",) <$> getEnvironment
readEnvVarInput _ (FromFlag name envVar) = return (name, [envVar])
readEnvVarInput _ (FromFileArgument fpa) = (showFilePathArgument fpa,) <$> (parseDotEnvFile =<< getFilePath fpa)
-- Wasp's dotenv files are optional, so a missing one simply contributes no env vars.
readEnvVarInput projectDir (FromProjectFile file) =
  (SP.fromRelFile file,) <$> (maybe (return []) parseDotEnvFile =<< findFileInWaspProjectDir projectDir file)

-- | Merges all the env vars from the given sources, ensuring that none of the
-- first argument env vars are overridden by the second argument env vars;
-- otherwise, returns an error message.
resolveEnvVars :: [EnvVar] -> [(SourceName, [EnvVar])] -> Either String [EnvVar]
resolveEnvVars forcedEnvVars envVarsBySource =
  case assertNoOverriddenEnvVars forcedEnvVars envVarsBySource of
    Nothing -> Right $ nubEnvVars $ forcedEnvVars ++ concatMap snd envVarsBySource
    Just errMsg -> Left errMsg

assertNoOverriddenEnvVars :: [EnvVar] -> [(SourceName, [EnvVar])] -> Maybe String
assertNoOverriddenEnvVars forcedEnvVars envVarsBySource
  | null overriddenEnvVars = Nothing
  | otherwise =
      Just $
        "The following env vars are set by Wasp and cannot be overridden by the user: "
          ++ intercalate
            ", "
            ( map
                (\(source, name) -> name ++ " (set in " ++ source ++ ")")
                overriddenEnvVars
            )
  where
    -- Each forced env var the user also set, paired with the source they set it in.
    overriddenEnvVars =
      [ (sourceName, envVarName)
      | (sourceName, envVars) <- envVarsBySource,
        (envVarName, _) <- envVars,
        envVarName `Set.member` forcedEnvVarNames
      ]

    forcedEnvVarNames = Set.fromList $ map fst forcedEnvVars
