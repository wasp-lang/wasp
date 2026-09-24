-- | This modules implements general concepts regarding env vars.
-- It is not specific to Wasp in any way.
module Wasp.Env
  ( EnvVar,
    EnvVarName,
    EnvVarValue,
    parseDotEnvFileLikeNodeDotenv,
    envVarsToDotEnvContent,
    nubEnvVars,
    formatEnvVarValue,
    findDuplicateEnvVars,
    HasEnvVars (..),
    addEnvVarsUnique,
    addEnvVarsOverride,
  )
where

import Data.Function (on)
import Data.List (intercalate, nubBy)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, File, Path', fromAbsFile)
import Wasp.Env.NodeDotenvParser (parseDotEnvContentLikeNodeDotenv)

type EnvVar = (EnvVarName, EnvVarValue)

type EnvVarName = String

type EnvVarValue = String

-- | Reads the specified dotenv file and returns its env vars, the same way
-- the generated apps would read it with Node's @dotenv@ (see
-- "Wasp.Env.NodeDotenvParser"). Crashes if the file doesn't exist.
parseDotEnvFileLikeNodeDotenv :: Path' Abs (File ()) -> IO [EnvVar]
parseDotEnvFileLikeNodeDotenv envFile =
  parseDotEnvContentLikeNodeDotenv . T.unpack <$> T.IO.readFile (fromAbsFile envFile)

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
  setEnvVars :: a -> [EnvVar] -> a

-- | Combines the existing env vars of a type with new env vars. If there are
-- duplicates in the new env vars, returns a @Left@ of the duplicate env var
-- names.
addEnvVarsUnique :: (HasEnvVars a) => a -> [EnvVar] -> Either (Set EnvVarName) a
addEnvVarsUnique x incoming
  | Set.null duplicateNames = Right $ addEnvVarsOverride x incoming
  | otherwise = Left duplicateNames
  where
    duplicateNames = findDuplicateEnvVars existing incoming
    existing = getEnvVars x

-- | Combines the existing env vars of a type with new env vars. If there are
-- duplicates in the new env vars, the new env vars will override the existing
-- ones.
addEnvVarsOverride :: (HasEnvVars a) => a -> [EnvVar] -> a
addEnvVarsOverride x incoming = setEnvVars x $ nubEnvVars merged
  where
    merged =
      -- Incoming first so that they take priority over existing.
      incoming <> existing
    existing = getEnvVars x
