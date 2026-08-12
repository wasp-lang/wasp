{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Util.EnvVarSource where

import Control.Monad.Except (throwError)
import Data.List (find, intercalate)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath, showFilePathArgument)
import Wasp.Env (EnvVar, EnvVarName, nubEnvVars, parseDotEnvFile)

type EnvVarSource = (String, [EnvVar])

resolveEnvVarArguments :: [EnvVar] -> EnvVarSource
resolveEnvVarArguments = ("CLI arguments",)

resolveEnvVarFile :: FilePathArgument -> IO EnvVarSource
resolveEnvVarFile filePath =
  ("file " ++ showFilePathArgument filePath,)
    <$> (parseDotEnvFile =<< getFilePath filePath)

-- | Runs a function that takes a list of environment variables, while also
-- checking that none of the variables are overridden by Wasp itself. If any
-- are overridden, a CommandError is thrown.
withEnvVarSources :: [EnvVarSource] -> ([EnvVar] -> Either [EnvVarName] a) -> Command a
withEnvVarSources sources f =
  case f $ toEnvVarList sources of
    Left overriddenNames -> throwOverriddenVarsError sources overriddenNames
    Right result -> return result

throwOverriddenVarsError :: [EnvVarSource] -> [EnvVarName] -> Command a
throwOverriddenVarsError sources overriddenNames =
  throwError $
    CommandError "Overridden environment variables" $
      "Wasp sets the following environment variables, so you must not specify them yourself: "
        ++ intercalate ", " (describeOverriddenEnvVar <$> overriddenEnvVars)
  where
    overriddenEnvVars =
      [ (name, findSourceForEnvVar name)
      | name <- overriddenNames
      ]

    describeOverriddenEnvVar (name, Nothing) = name
    describeOverriddenEnvVar (name, Just source) = name ++ " (set in " ++ source ++ ")"

    findSourceForEnvVar :: EnvVarName -> Maybe String
    findSourceForEnvVar name =
      fst <$> find (\(_, envVars) -> name `elem` (fst <$> envVars)) sources

toEnvVarList :: [EnvVarSource] -> [EnvVar]
toEnvVarList = nubEnvVars . concatMap snd
