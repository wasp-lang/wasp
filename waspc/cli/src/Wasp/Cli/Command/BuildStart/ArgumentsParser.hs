module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( parseBuildStartArgs,
    BuildStartArgs (..),
  )
where

import Options.Applicative (defaultPrefs, execParserPure)
import qualified Options.Applicative as Opt
import Wasp.Cli.Command.Call (Arguments)

data BuildStartArgs = BuildStartArgs
  { serverEnvironmentVariables :: [String],
    serverEnvironmentFiles :: [FilePath]
  }

parseBuildStartArgs :: Arguments -> Either String BuildStartArgs
parseBuildStartArgs newArgs = parserResultToEither $ execParserPure defaultPrefs buildStartArgsParserInfo newArgs
  where
    buildStartArgsParserInfo :: Opt.ParserInfo BuildStartArgs
    buildStartArgsParserInfo = Opt.info (buildStartArgsParser Opt.<**> Opt.helper) Opt.fullDesc

    buildStartArgsParser :: Opt.Parser BuildStartArgs
    buildStartArgsParser =
      BuildStartArgs
        <$> Opt.many serverEnvironmentVariableParser
        <*> Opt.many serverEnvironmentFileParser

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

    parserResultToEither :: Opt.ParserResult BuildStartArgs -> Either String BuildStartArgs
    parserResultToEither (Opt.Success success) = Right success
    parserResultToEither (Opt.Failure failure) = Left $ show help
      where
        (help, _, _) = Opt.execFailure failure "wasp build start"
    parserResultToEither (Opt.CompletionInvoked _) = error "Completion invoked when parsing 'wasp build start', but this should never happen"
