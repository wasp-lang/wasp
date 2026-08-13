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
    -- One container runs the whole app, so these two groups are not two
    -- programs: the client ones are baked into the app's pages and assets while
    -- they are built, and the server ones are given to the running app.
    clientEnvironmentVariableParser =
      makeEnvironmentVariableParser clientTargetDescription "client-env" 'c'
    clientEnvironmentFileParser =
      makeEnvironmentFileParser clientTargetDescription "client-env-file"

    serverEnvironmentVariableParser =
      makeEnvironmentVariableParser serverTargetDescription "server-env" 's'
    serverEnvironmentFileParser =
      makeEnvironmentFileParser serverTargetDescription "server-env-file"

    clientTargetDescription = "baked into the app's pages and assets when they are built"
    serverTargetDescription = "given to the app while it runs"

    makeEnvironmentVariableParser :: String -> String -> Char -> Opt.Parser EnvVar
    makeEnvironmentVariableParser targetDescription longOptionName shortOptionName =
      Opt.option envVarReader $
        Opt.long longOptionName
          <> Opt.short shortOptionName
          <> Opt.metavar "NAME=VALUE"
          <> Opt.help ("Set an environment variable " <> targetDescription <> " (can be used multiple times)")

    makeEnvironmentFileParser :: String -> String -> Opt.Parser FilePathArgument
    makeEnvironmentFileParser targetDescription longOptionName =
      Opt.option filePathReader $
        Opt.long longOptionName
          <> Opt.metavar "FILE_PATH"
          <> Opt.help ("Load environment variables " <> targetDescription <> " from a file (can be used multiple times)")
          <> Opt.action "file"
