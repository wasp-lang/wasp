{-# LANGUAGE TupleSections #-}

module Wasp.Cli.EnvVarCtx where

import Control.Monad.Except (throwError)
import Data.Function ((&))
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import StrongPath (Abs, Dir, File, File', Path', Rel)
import System.Environment (getEnvironment)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Util.PathArgument (FilePathArgument, getFilePath)
import Wasp.Env (EnvVar, EnvVarName, HasEnvVars, addEnvVarsUnique, parseDotEnvFile)
import Wasp.Project.Common (WaspProjectDir, findFileInWaspProjectDir)

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

-- | A dotenv file Wasp looks for in the project dir, e.g. @.env.server@. These
-- files are optional, so a missing one simply contributes no env vars.
fromProjectFile ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO [EnvVarWithCtx]
fromProjectFile projectDir file = do
  maybeDotEnvFile <- findFileInWaspProjectDir projectDir file
  case maybeDotEnvFile of
    Nothing -> return []
    Just filePath -> fromDotEnvFile (show file) filePath

fromDotEnvFile :: String -> Path' Abs (File ()) -> IO [EnvVarWithCtx]
fromDotEnvFile fileName filePath =
  fmap (EnvVarCtx {sourceName = "file " ++ fileName},) <$> parseDotEnvFile filePath

-- | The environment Wasp itself was started in.
fromCurrentEnvironment :: IO [EnvVarWithCtx]
fromCurrentEnvironment = fmap (EnvVarCtx {sourceName = "your environment"},) <$> getEnvironment

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
