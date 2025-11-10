module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Util.EnvVarArgument (envVarReader)
import Wasp.Cli.Util.PathArgument (FilePathArgument, filePathReader)
import Wasp.Env (EnvVar)

data BuildStartArgs = BuildStartArgs
  { clientEnvironmentVariables :: [EnvVar],
    clientEnvironmentFiles :: [FilePathArgument],
    serverEnvironmentVariables :: [EnvVar],
    serverEnvironmentFiles :: [FilePathArgument]
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> Opt.many clientEnvironmentVariableParser
    <*> Opt.many clientEnvironmentFileParser
    <*> Opt.many serverEnvironmentVariableParser
    <*> Opt.many serverEnvironmentFileParser
  where
    clientEnvironmentVariableParser =
      makeEnvironmentVariableParser "client" "client-env" 'c'
    clientEnvironmentFileParser =
      makeEnvironmentFileParser "client" "client-env-file"

    serverEnvironmentVariableParser =
      makeEnvironmentVariableParser "server" "server-env" 's'
    serverEnvironmentFileParser =
      makeEnvironmentFileParser "server" "server-env-file"

    makeEnvironmentVariableParser :: String -> String -> Char -> Opt.Parser EnvVar
    makeEnvironmentVariableParser targetName longOptionName shortOptionName =
      Opt.option envVarReader $
        Opt.long longOptionName
          <> Opt.short shortOptionName
          <> Opt.metavar "NAME=VALUE"
          <> Opt.help ("Set an environment variable for the " <> targetName <> " (can be used multiple times)")

    makeEnvironmentFileParser :: String -> String -> Opt.Parser FilePathArgument
    makeEnvironmentFileParser targetName longOptionName =
      Opt.option filePathReader $
        Opt.long longOptionName
          <> Opt.metavar "FILE_PATH"
          <> Opt.help ("Load environment variables for the " <> targetName <> " from a file (can be used multiple times)")
          <> Opt.action "file"
