-- | This modules implements general concepts regarding env vars.
-- It is not specific to Wasp in any way.
module Wasp.Env
  ( EnvVar,
    EnvVarName,
    EnvVarValue,
    parseDotEnvFile,
    parseDotEnvFilePath,
    envVarsToDotEnvContent,
    envVarFromString,
    nubEnvVars,
  )
where

import qualified Configuration.Dotenv as Dotenv
import Control.Exception (ErrorCall (ErrorCall))
import Data.Function (on)
import Data.List (intercalate, nubBy)
import qualified Data.Text as T
import StrongPath (Abs, File, Path', fromAbsFile)
import UnliftIO.Exception (catch, throwIO)

type EnvVar = (EnvVarName, EnvVarValue)

type EnvVarName = String

type EnvVarValue = String

parseDotEnvFile :: Path' Abs (File ()) -> IO [EnvVar]
parseDotEnvFile = parseDotEnvFilePath . fromAbsFile

-- Reads the specified dotenv file and returns its values.
-- Crashes if file doesn't exist or it can't parse it.
parseDotEnvFilePath :: FilePath -> IO [EnvVar]
parseDotEnvFilePath envFilePath =
  Dotenv.parseFile envFilePath
    -- Parse errors are returned from Dotenv.parseFile as ErrorCall, which Wasp compiler would
    -- report as a bug in compiler, so we instead convert these to IOExceptions.
    `catch` \(ErrorCall msg) -> throwIO $ userError $ "Failed to parse dot env file: " <> msg

envVarsToDotEnvContent :: [EnvVar] -> T.Text
envVarsToDotEnvContent vars =
  T.pack $ intercalate "\n" $ map (\(name, value) -> name <> "=" <> show value) vars

envVarFromString :: String -> Either String EnvVar
envVarFromString var =
  case break (== '=') var of
    ([], _) -> failure
    (name, '=' : value) -> Right (name, value)
    _ -> failure
  where
    failure = Left $ "Environment variable must be in the format NAME=VALUE: " ++ var

nubEnvVars :: [EnvVar] -> [EnvVar]
nubEnvVars = nubBy ((==) `on` fst)
