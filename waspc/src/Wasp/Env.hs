{-# LANGUAGE FlexibleInstances #-}

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
    HasEnvVars (..),
    addEnvVarsUnique,
    addEnvVarsOverride,
    inheritEnvWith,
  )
where

import qualified Configuration.Dotenv as Dotenv
import Control.Exception (ErrorCall (ErrorCall))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import StrongPath (Abs, File, Path', fromAbsFile)
import System.Environment (getEnvironment)
import qualified System.Process as P
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

class HasEnvVars a where
  getEnvVars :: a -> [EnvVar]
  setEnvVars :: [EnvVar] -> a -> a

instance HasEnvVars [EnvVar] where
  getEnvVars = id
  setEnvVars newEnvVars _ = newEnvVars

instance HasEnvVars P.CreateProcess where
  getEnvVars process = fromMaybe [] (P.env process)
  setEnvVars newEnvVars process = process {P.env = Just newEnvVars}

-- | Combines the existing env vars of a type with new env vars. If there are
-- duplicates in the new env vars, returns a @Left@ of the duplicate env var
-- names.
addEnvVarsUnique :: (HasEnvVars a) => [EnvVar] -> a -> Either (Set EnvVarName) a
addEnvVarsUnique incoming x
  | Set.null duplicateNames = Right $ addEnvVarsOverride incoming x
  | otherwise = Left duplicateNames
  where
    duplicateNames = findDuplicateEnvVars existing incoming
    existing = getEnvVars x

-- | Combines the existing env vars of a type with new env vars. If there are
-- duplicates in the new env vars, the new env vars will override the existing
-- ones.
addEnvVarsOverride :: (HasEnvVars a) => [EnvVar] -> a -> a
addEnvVarsOverride incoming x = setEnvVars (nubEnvVars merged) x
  where
    merged =
      -- Incoming first so that they take priority over existing.
      incoming <> existing
    existing = getEnvVars x

-- | Sets the process's env vars to the ones of the current process, combined
-- with the given env vars, which take priority.
inheritEnvWith :: (MonadIO m, HasEnvVars a) => [EnvVar] -> a -> m a
inheritEnvWith extraEnvVars x = liftIO $ do
  environment <- getEnvironment
  return $ addEnvVarsOverride extraEnvVars $ setEnvVars environment x
