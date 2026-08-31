{-# LANGUAGE TupleSections #-}

module Wasp.Cli.EnvVarWithCtx
  ( EnvVarWithCtx,
    EnvVarCtx (..),
    showEnvVarWithCtx,
    addEnvVarsUniqueC,
    readEnvVarArgument,
    readDotEnvFile,
    readEnvironment,
  )
where

import Control.Monad.Except (throwError)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import StrongPath (Abs, File, Path')
import System.Environment (getEnvironment)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.EnvVarArgument (EnvVarArgument (..))
import Wasp.Cli.Util.PathArgument (getFilePath)
import Wasp.Env (EnvVar, EnvVarName, HasEnvVars, addEnvVarsUnique, parseDotEnvFile)

type EnvVarWithCtx = (EnvVarCtx, EnvVar)

newtype EnvVarCtx = EnvVarCtx
  { -- | Where the environment variable was set, e.g. "CLI arguments" or ".env file".
    sourceDescription :: String
  }

readEnvVarArgument :: EnvVarArgument -> IO [EnvVarWithCtx]
readEnvVarArgument (EnvVarArgumentLiteral envVar) =
  return [(EnvVarCtx {sourceDescription = "CLI arguments"}, envVar)]
readEnvVarArgument (EnvVarArgumentFile filePathArg) =
  getFilePath filePathArg
    >>= readDotEnvFile (show filePathArg)

readDotEnvFile :: String -> Path' Abs (File ()) -> IO [EnvVarWithCtx]
readDotEnvFile fileDescription filePath =
  fmap (EnvVarCtx {sourceDescription = "file " ++ fileDescription},) <$> parseDotEnvFile filePath

readEnvironment :: IO [EnvVarWithCtx]
readEnvironment =
  fmap (EnvVarCtx {sourceDescription = "your environment"},) <$> getEnvironment

showEnvVarWithCtx :: EnvVarWithCtx -> String
showEnvVarWithCtx (EnvVarCtx {sourceDescription}, (envVarName, _)) =
  envVarName ++ " (received from " ++ sourceDescription ++ ")"

addEnvVarsUniqueC :: (HasEnvVars a) => a -> [EnvVarWithCtx] -> Command a
addEnvVarsUniqueC x incomingEnvVarSources =
  addEnvVarsUnique x incomingEnvVars
    & either (throwOverriddenVarsError incomingEnvVarSources) return
  where
    incomingEnvVars = snd <$> incomingEnvVarSources

throwOverriddenVarsError :: [EnvVarWithCtx] -> Set EnvVarName -> Command a
throwOverriddenVarsError envVarsWithCtx overriddenNames =
  throwError $
    CommandError "Can't override managed environment variables" $
      "You specified the following environment variables, but Wasp sets them: "
        ++ intercalate ", " (showEnvVarWithCtx <$> overriddenEnvVars)
        ++ ". Please remove them so they don't cause conflicts when running."
  where
    overriddenEnvVars = filter (\(_, (name, _)) -> name `Set.member` overriddenNames) envVarsWithCtx
