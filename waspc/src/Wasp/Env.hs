-- | This modules implements general concepts regarding env vars.
-- It is not specific to Wasp in any way.
module Wasp.Env
  ( EnvVar,
    EnvVarName,
    EnvVarValue,
    parseDotEnvFile,
    envVarsToDotEnvContent,
    nubEnvVars,
    formatEnvVarValue,
    findDuplicateEnvVars,
    addEnvVarsUnique,
    addEnvVarsOverride,
  )
where

import qualified Configuration.Dotenv as Dotenv
import Control.Exception (ErrorCall (ErrorCall))
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Set (Set)
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

findDuplicateEnvVars :: [EnvVar] -> [EnvVar] -> Set EnvVarName
findDuplicateEnvVars existing incoming =
  existingNames `Set.intersection` incomingNames
  where
    existingNames = Set.fromList $ fst <$> existing
    incomingNames = Set.fromList $ fst <$> incoming

-- | Combines existing env vars with incoming ones. If there are duplicates in
-- the incoming env vars, returns a @Left@ of the duplicate env var names.
addEnvVarsUnique :: [EnvVar] -> [EnvVar] -> Either (Set EnvVarName) [EnvVar]
addEnvVarsUnique incoming existing
  | Set.null duplicateNames = Right $ addEnvVarsOverride incoming existing
  | otherwise = Left duplicateNames
  where
    duplicateNames = findDuplicateEnvVars existing incoming

-- | Combines existing env vars with incoming ones. If there are duplicates in
-- the incoming env vars, they override the existing ones.
addEnvVarsOverride :: [EnvVar] -> [EnvVar] -> [EnvVar]
addEnvVarsOverride incoming existing =
  -- Incoming first so that they take priority over existing.
  nubEnvVars $ incoming <> existing
