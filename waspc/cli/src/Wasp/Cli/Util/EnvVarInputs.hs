{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Util.EnvVarInputs
  ( EnvVarInput (..),
    SourceName,
    EnvVarsBySource,
    readEnvVarInput,
    resolveEnvVarInputs,
    mergeEnvVars,
    describeEnvVarSources,
  )
where

import Data.List (intercalate)
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath, showFilePathArgument)
import Wasp.Env (EnvVar, EnvVarName, nubEnvVars, parseDotEnvFile)
import Wasp.Project.Common (WaspProjectDir, findFileInWaspProjectDir)

-- | A user-facing description of where some env vars came from, e.g. a file
-- path or a CLI option.
type SourceName = String

-- | The env vars the user gave us, still grouped by the input they came from so
-- that we can point back at it in error messages.
type EnvVarsBySource = [(SourceName, [EnvVar])]

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

resolveEnvVarInputs :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> [EnvVarInput] -> IO EnvVarsBySource
resolveEnvVarInputs projectDir = mapM (readEnvVarInput projectDir)

readEnvVarInput :: SP.Path' SP.Abs (SP.Dir WaspProjectDir) -> EnvVarInput -> IO (SourceName, [EnvVar])
readEnvVarInput _ Inherit = ("your environment",) <$> getEnvironment
readEnvVarInput _ (FromFlag name envVar) = return (name, [envVar])
readEnvVarInput _ (FromFileArgument fpa) = (showFilePathArgument fpa,) <$> (parseDotEnvFile =<< getFilePath fpa)
-- Wasp's dotenv files are optional, so a missing one simply contributes no env vars.
readEnvVarInput projectDir (FromProjectFile file) =
  (SP.fromRelFile file,) <$> (maybe (return []) parseDotEnvFile =<< findFileInWaspProjectDir projectDir file)

-- | Merges the env vars from all the sources, letting the earlier sources win
-- over the later ones.
mergeEnvVars :: EnvVarsBySource -> [EnvVar]
mergeEnvVars = nubEnvVars . concatMap snd

-- | Lists the given env vars along with the sources that set them, e.g.
-- @PORT (set in .env.server), DATABASE_URL (set in --server-env)@.
describeEnvVarSources :: EnvVarsBySource -> [EnvVarName] -> String
describeEnvVarSources envVarsBySource names =
  intercalate ", " [name ++ " (set in " ++ sourcesOf name ++ ")" | name <- names]
  where
    sourcesOf name =
      intercalate " and " [source | (source, envVars) <- envVarsBySource, name `elem` map fst envVars]
