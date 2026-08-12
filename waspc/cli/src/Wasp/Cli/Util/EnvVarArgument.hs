module Wasp.Cli.Util.EnvVarArgument
  ( envVarReader,
    envVarFromString,
    envVarArgumentsParser,
    envVarFilesParser,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Util.PathArgument (FilePathArgument, filePathReader)
import Wasp.Env (EnvVar)

envVarArgumentsParser :: String -> Char -> String -> Opt.Parser [EnvVar]
envVarArgumentsParser targetName shortOptionName longOptionName =
  Opt.many $
    Opt.option envVarReader $
      Opt.long longOptionName
        <> Opt.short shortOptionName
        <> Opt.metavar "NAME=VALUE"
        <> Opt.help ("Set an environment variable for the " <> targetName <> " (can be used multiple times)")

envVarReader :: Opt.ReadM EnvVar
envVarReader = Opt.eitherReader envVarFromString

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

envVarFilesParser :: String -> String -> Opt.Parser [FilePathArgument]
envVarFilesParser targetName fileOptionName =
  Opt.many $
    Opt.option filePathReader $
      Opt.long fileOptionName
        <> Opt.metavar "FILE_PATH"
        <> Opt.help ("Load environment variables for the " <> targetName <> " from a file (can be used multiple times)")
        <> Opt.action "file"
