{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Util.EnvVarSource where

import Control.Monad.Except (throwError)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.List.NonEmpty.Extra (nonEmpty)
import qualified StrongPath as SP
import System.Environment (getEnvironment)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath, showFilePathArgument)
import Wasp.Env (EnvVar, EnvVarName, findDuplicateEnvVars, overrideEnvVars, parseDotEnvFile)
import Wasp.Project.Common (WaspProjectDir, findFileInWaspProjectDir)

type EnvVarSource = (String, [EnvVar])

resolveEnvVarArguments :: [EnvVar] -> EnvVarSource
resolveEnvVarArguments = ("CLI arguments",)

resolveEnvVarFile :: FilePathArgument -> IO EnvVarSource
resolveEnvVarFile filePath =
  ("file " ++ showFilePathArgument filePath,)
    <$> (parseDotEnvFile =<< getFilePath filePath)

-- | A dotenv file Wasp looks for in the project dir, e.g. @.env.server@. These
-- files are optional, so a missing one simply contributes no env vars.
resolveEnvVarProjectFile ::
  SP.Path' SP.Abs (SP.Dir WaspProjectDir) ->
  SP.Path' (SP.Rel WaspProjectDir) SP.File' ->
  IO EnvVarSource
resolveEnvVarProjectFile projectDir file =
  ("file " ++ SP.fromRelFile file,)
    <$> (maybe (return []) parseDotEnvFile =<< findFileInWaspProjectDir projectDir file)

-- | The environment Wasp itself was started in.
resolveInheritedEnvVars :: IO EnvVarSource
resolveInheritedEnvVars = ("your environment",) <$> getEnvironment

overrideEnvVarsC :: [EnvVar] -> [EnvVarSource] -> Command [EnvVar]
overrideEnvVarsC existingEnvVars incomingEnvVarSources =
  either (throwOverriddenVarsError incomingEnvVarSources) return $
    overrideEnvVars existingEnvVars (concatMap snd incomingEnvVarSources)

assertNoOverriddenEnvVars :: [EnvVar] -> [EnvVarSource] -> Command ()
assertNoOverriddenEnvVars existingEnvVars incomingEnvVarSources =
  maybe (return ()) (throwOverriddenVarsError incomingEnvVarSources) $
    nonEmpty (findDuplicateEnvVars existingEnvVars $ concatMap snd incomingEnvVarSources)

throwOverriddenVarsError :: [EnvVarSource] -> NonEmpty EnvVarName -> Command a
throwOverriddenVarsError sources overriddenNames =
  throwError $
    CommandError "Overridden environment variables" $
      "Wasp sets the following environment variables, so you must not specify them yourself: "
        ++ intercalate ", " (describeOverriddenEnvVar <$> overriddenEnvVars)
  where
    overriddenEnvVars =
      [ (name, findSourceForEnvVar name)
      | name <- toList overriddenNames
      ]

    describeOverriddenEnvVar (name, Nothing) = name
    describeOverriddenEnvVar (name, Just source) = name ++ " (set in " ++ source ++ ")"

    findSourceForEnvVar :: EnvVarName -> Maybe String
    findSourceForEnvVar name =
      fst <$> find (\(_, envVars) -> name `elem` (fst <$> envVars)) sources
