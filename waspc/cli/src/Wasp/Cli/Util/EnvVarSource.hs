{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Util.EnvVarSource where

import Control.Monad.Except (throwError)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.List.NonEmpty.Extra (nonEmpty)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath, showFilePathArgument)
import Wasp.Env (EnvVar, EnvVarName, findDuplicateEnvVars, parseDotEnvFile)
import Wasp.Generator.RunConfig (HasEnvVars, addEnvVars)

type EnvVarSource = (String, [EnvVar])

resolveEnvVarArguments :: [EnvVar] -> EnvVarSource
resolveEnvVarArguments = ("CLI arguments",)

resolveEnvVarFile :: FilePathArgument -> IO EnvVarSource
resolveEnvVarFile filePath =
  ("file " ++ showFilePathArgument filePath,)
    <$> (parseDotEnvFile =<< getFilePath filePath)

addEnvVarsC :: (HasEnvVars a) => a -> [EnvVarSource] -> Command a
addEnvVarsC x incomingEnvVarSources =
  either
    (throwOverriddenVarsError incomingEnvVarSources)
    return
    (addEnvVars x incomingEnvVars)
  where
    incomingEnvVars = concatMap snd incomingEnvVarSources

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
