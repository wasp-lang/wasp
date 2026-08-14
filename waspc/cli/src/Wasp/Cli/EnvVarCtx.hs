{-# LANGUAGE TupleSections #-}

module Wasp.Cli.EnvVarCtx where

import Control.Monad.Except (throwError)
import Data.List (find, intercalate)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.List.NonEmpty.Extra (nonEmpty)
import StrongPath (Abs, File, Path')
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath)
import Wasp.Env (EnvVar, EnvVarName, HasEnvVars, addEnvVarsUnique, findDuplicateEnvVars, parseDotEnvFile)

type EnvVarWithCtx = (EnvVarCtx, EnvVar)

newtype EnvVarCtx = EnvVarCtx
  { -- | Where the environment variable was set, e.g. "CLI arguments" or ".env file".
    sourceName :: String
  }

fromCliArguments :: EnvVar -> EnvVarWithCtx
fromCliArguments = (EnvVarCtx {sourceName = "CLI arguments"},)

fromFilePathArgument :: FilePathArgument -> IO [EnvVarWithCtx]
fromFilePathArgument filePathArg = fromDotEnvFile fileName =<< filePath
  where
    filePath = getFilePath filePathArg
    fileName = show filePathArg

fromDotEnvFile :: String -> Path' Abs (File ()) -> IO [EnvVarWithCtx]
fromDotEnvFile fileName filePath =
  fmap (EnvVarCtx {sourceName = "file " ++ fileName},) <$> parseDotEnvFile filePath

addEnvVarsUniqueC :: (HasEnvVars a) => a -> [EnvVarWithCtx] -> Command a
addEnvVarsUniqueC x incomingEnvVarSources =
  either
    (throwOverriddenVarsError incomingEnvVarSources)
    return
    (addEnvVarsUnique x incomingEnvVars)
  where
    incomingEnvVars = snd <$> incomingEnvVarSources

assertNoOverriddenEnvVars :: [EnvVar] -> [EnvVarWithCtx] -> Command ()
assertNoOverriddenEnvVars existingEnvVars incomingEnvVarSources =
  maybe (return ()) (throwOverriddenVarsError incomingEnvVarSources) $
    nonEmpty (findDuplicateEnvVars existingEnvVars $ snd <$> incomingEnvVarSources)

throwOverriddenVarsError :: [EnvVarWithCtx] -> NonEmpty EnvVarName -> Command a
throwOverriddenVarsError envVarsWithCtx overriddenNames =
  throwError $
    CommandError "Overridden environment variables" $
      "The following environment variables will be overwritten by Wasp and should be removed: "
        ++ intercalate ", " (describeOverriddenEnvVar <$> overriddenEnvVars)
        ++ "."
  where
    overriddenEnvVars =
      [ (name, sourceName <$> findCtxForEnvVar name)
      | name <- toList overriddenNames
      ]

    describeOverriddenEnvVar (name, Nothing) = name
    describeOverriddenEnvVar (name, Just source) = name ++ " (received from " ++ source ++ ")"

    findCtxForEnvVar name =
      fst <$> find (\(_, (envVarName, _)) -> envVarName == name) envVarsWithCtx
