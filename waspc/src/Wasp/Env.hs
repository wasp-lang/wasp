-- | This modules implements general concepts regarding env vars.
-- It is not specific to Wasp in any way.
module Wasp.Env
  ( EnvVar,
    EnvVarName,
    EnvVarValue,
    parseDotEnvFile,
    envVarsToDotEnvContent,
    nubEnvVars,
    overrideEnvVars,
    formatEnvVarValue,
  )
where

import qualified Configuration.Dotenv as Dotenv
import Control.Exception (ErrorCall (ErrorCall))
import Data.Function (on)
import Data.List (intercalate, nubBy)
import qualified Data.Set as Set
import qualified Data.Text as T
import StrongPath (Abs, File, Path', fromAbsFile)
import UnliftIO.Exception (catch, throwIO)

type EnvVar = (EnvVarName, EnvVarValue)

type EnvVarName = String

type EnvVarValue = String

-- Reads the specified dotenv file and returns its values.
-- Crashes if file doesn't exist or it can't parse it.
parseDotEnvFile :: Path' Abs (File ()) -> IO [EnvVar]
parseDotEnvFile envFile =
  Dotenv.parseFile (fromAbsFile envFile)
    -- Parse errors are returned from Dotenv.parseFile as ErrorCall, which Wasp compiler would
    -- report as a bug in compiler, so we instead convert these to IOExceptions.
    `catch` \(ErrorCall msg) -> throwIO $ userError $ "Failed to parse dot env file: " <> msg

-- | Formats environment variables for .env file content.
envVarsToDotEnvContent :: [EnvVar] -> T.Text
envVarsToDotEnvContent vars =
  T.pack $ intercalate "\n" $ map formatEnvVar vars
  where
    formatEnvVar (name, value) = name <> "=" <> formatEnvVarValue value

formatEnvVarValue :: EnvVarValue -> EnvVarValue
formatEnvVarValue rawValue
  | needsQuoting rawValue = concat ["\"", rawValue, "\""]
  | otherwise = rawValue
  where
    needsQuoting :: String -> Bool
    needsQuoting val = ' ' `elem` val

nubEnvVars :: [EnvVar] -> [EnvVar]
nubEnvVars = nubBy ((==) `on` fst)

-- | If any of the first env vars already exist in the second list, returns
-- `Left` of the list of duplicated env var names. Otherwise returns `Right` of
-- the combined list.
--
-- This function is useful to add internal environment variables to a list of
-- user-defined ones. If the user has already defined an environment variable
-- with the same name, we'll want to inform them so that the user doesn't get
-- confused about which value is being used. If the user has not defined that
-- environment variable, we just prepend it to the list and continue.
overrideEnvVars :: [EnvVar] -> [EnvVar] -> Either [EnvVarName] [EnvVar]
left `overrideEnvVars` right =
  if null duplicateNames
    then Right (left <> right)
    else Left duplicateNames
  where
    duplicateNames = filter (`Set.member` leftNames) rightNames
    leftNames = Set.fromList $ map fst left
    rightNames = map fst right
