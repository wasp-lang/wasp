module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import qualified Options.Applicative as Opt

data BuildStartArgs = BuildStartArgs
  { serverEnvironmentVariables :: [String],
    serverEnvironmentFiles :: [FilePath]
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> Opt.many serverEnvironmentVariableParser
    <*> Opt.many serverEnvironmentFileParser
  where
    serverEnvironmentVariableParser :: Opt.Parser String
    serverEnvironmentVariableParser =
      Opt.strOption $
        Opt.long "server-env"
          <> Opt.short 'e'
          <> Opt.metavar "NAME=VALUE"
          <> Opt.help "Set an environment variable for the server"

    serverEnvironmentFileParser :: Opt.Parser FilePath
    serverEnvironmentFileParser =
      Opt.strOption $
        Opt.long "server-env-file"
          <> Opt.metavar "FILE_PATH"
          <> Opt.help "Load environment variables for the server from a file"
          <> Opt.action "file"
