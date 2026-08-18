{-# LANGUAGE TupleSections #-}

module Wasp.Cli.EnvVarWithCtx
  ( EnvVarWithCtx,
    EnvVarCtx (..),
    showEnvVarWithCtx,
    addEnvVarsUniqueC,
    readEnvVarArgument,
  )
where

import Control.Monad.Except (throwError)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import StrongPath (Abs, File, Path')
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.EnvVarArgument (EnvVarArgument (..))
import Wasp.Cli.Util.PathArgument (getFilePath)
import Wasp.Env (EnvVar, EnvVarName, HasEnvVars, addEnvVarsUnique, parseDotEnvFile)

type EnvVarWithCtx = (EnvVarCtx, EnvVar)

newtype EnvVarCtx = EnvVarCtx
  { -- | Where the environment variable was set, e.g. "CLI arguments" or ".env file".
    sourceName :: String
  }

readEnvVarArgument :: EnvVarArgument -> IO [EnvVarWithCtx]
readEnvVarArgument (LiteralCliArgument envVar) =
  return [(EnvVarCtx {sourceName = "CLI arguments"}, envVar)]
readEnvVarArgument (FileArgument filePathArg) =
  fromDotEnvFile fileName =<< filePath
  where
    filePath = getFilePath filePathArg
    fileName = show filePathArg

fromDotEnvFile :: String -> Path' Abs (File ()) -> IO [EnvVarWithCtx]
fromDotEnvFile fileName filePath =
  fmap (EnvVarCtx {sourceName = "file " ++ fileName},) <$> parseDotEnvFile filePath

showEnvVarWithCtx :: EnvVarWithCtx -> [Char]
showEnvVarWithCtx (EnvVarCtx {sourceName = s}, (name, _)) =
  name ++ " (received from " ++ s ++ ")"

addEnvVarsUniqueC :: (HasEnvVars a) => a -> [EnvVarWithCtx] -> Command a
addEnvVarsUniqueC x incomingEnvVarSources =
  addEnvVarsUnique x incomingEnvVars
    & either (throwOverriddenVarsError incomingEnvVarSources) return
  where
    incomingEnvVars = snd <$> incomingEnvVarSources

throwOverriddenVarsError :: [EnvVarWithCtx] -> Set EnvVarName -> Command a
throwOverriddenVarsError envVarsWithCtx overriddenNames =
  throwError $
    CommandError "Can't override Wasp-managed environment variables" $
      "The following environment variables will be overwritten by Wasp and should be removed: "
        ++ intercalate ", " (showEnvVarWithCtx <$> overriddenEnvVars)
        ++ "."
  where
    overriddenEnvVars = filter (\(_, (name, _)) -> name `Set.member` overriddenNames) envVarsWithCtx
