module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import qualified Options.Applicative as Opt

data BuildStartArgs = BuildStartArgs
  { clientEnvironmentVariables :: [String],
    clientEnvironmentFiles :: [FilePath],
    serverEnvironmentVariables :: [String],
    serverEnvironmentFiles :: [FilePath]
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> Opt.many clientEnvironmentVariableParser
    <*> Opt.many clientEnvironmentFileParser
    <*> Opt.many serverEnvironmentVariableParser
    <*> Opt.many serverEnvironmentFileParser
  where
    clientEnvironmentVariableParser = makeEnvironmentVariableParser "client"
    clientEnvironmentFileParser = makeEnvironmentFileParser "client"
    serverEnvironmentVariableParser = makeEnvironmentVariableParser "server"
    serverEnvironmentFileParser = makeEnvironmentFileParser "server"

    makeEnvironmentVariableParser :: String -> Opt.Parser String
    makeEnvironmentVariableParser prefix =
      Opt.strOption $
        Opt.long (prefix <> "-env")
          <> Opt.short (head prefix)
          <> Opt.metavar "NAME=VALUE"
          <> Opt.help ("Set an environment variable for the " <> prefix <> " (can be used multiple times)")

    makeEnvironmentFileParser :: String -> Opt.Parser FilePath
    makeEnvironmentFileParser prefix =
      Opt.strOption $
        Opt.long (prefix <> "-env-file")
          <> Opt.metavar "FILE_PATH"
          <> Opt.help ("Load environment variables for the " <> prefix <> " from a file (can be used multiple times)")
          <> Opt.action "file"
