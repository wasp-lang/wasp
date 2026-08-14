module Wasp.Generator.RunConfig where

import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Wasp.Env (EnvVar, EnvVarName, findDuplicateEnvVars, nubEnvVars)

class HasEnvVars a where
  envVars :: a -> [EnvVar]
  replaceEnvVars :: a -> [EnvVar] -> a

-- | Combines the existing env vars of a type with new env vars. If there are
-- duplicates in the new env vars, returns a @Left@ of the duplicate env var
-- names.
addEnvVars :: (HasEnvVars a) => a -> [EnvVar] -> Either (NonEmpty EnvVarName) a
addEnvVars x incoming =
  case nonEmpty duplicateNames of
    Nothing -> Right $ overrideEnvVars x incoming
    Just names -> Left names
  where
    duplicateNames = findDuplicateEnvVars existing incoming
    existing = envVars x

-- | Combines the existing env vars of a type with new env vars. If there are
-- duplicates in the new env vars, the new env vars will override the existing
-- ones.
overrideEnvVars :: (HasEnvVars a) => a -> [EnvVar] -> a
overrideEnvVars x incoming = replaceEnvVars x $ nubEnvVars merged
  where
    merged =
      -- Incoming first so that they take priority over existing.
      incoming <> existing
    existing = envVars x
