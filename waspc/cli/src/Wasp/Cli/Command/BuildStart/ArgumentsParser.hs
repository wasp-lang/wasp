module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Data.Traversable (for)
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.EnvVarArgument (envVarReader)
import Wasp.Cli.Util.PathArgument (FilePathArgument, filePathReader)
import Wasp.Cli.Util.PortArgument (appPortsParser)
import Wasp.Env (EnvVar)
import Wasp.Project.Apps (Apps)
import qualified Wasp.Project.Apps as Apps

data BuildStartArgs = BuildStartArgs
  { envInputs :: Apps ([EnvVar], [FilePathArgument]),
    ports :: Apps PortNumber
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> envInputsParser
    <*> appPortsParser
  where
    envInputsParser = for Apps.names $ \name ->
      liftA2
        (,)
        (Opt.many $ makeEnvironmentVariableParser name (name ++ "-env") (head name))
        (Opt.many $ makeEnvironmentFileParser name (name ++ "-env-file"))

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
