module Wasp.Cli.Util.EnvVarArgument
  ( envVarReader,
    envVarFromString,
  )
where

import Options.Applicative (ReadM, eitherReader)
import Wasp.Env (EnvVar)

envVarReader :: ReadM EnvVar
envVarReader = eitherReader envVarFromString

-- | Converts a string to an EnvVar, throwing an error if the string is not in
-- the correct format. The input format is expected to be "NAME=VALUE".
envVarFromString :: String -> Either String EnvVar
envVarFromString var =
  case break (== '=') var of
    ([], _) -> failure
    (name, '=' : value) -> Right (name, value)
    _ -> failure
  where
    failure = Left $ "Environment variable must be in the format NAME=VALUE: " ++ var
