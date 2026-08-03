module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Data.Traversable (for)
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.EnvVarArgument (envVarReader)
import Wasp.Cli.Util.EnvVarInputs (EnvVarInput (..))
import Wasp.Cli.Util.PathArgument (filePathReader)
import Wasp.Cli.Util.PortArgument (appPortsParser)
import Wasp.Project.PerService (PerService)
import qualified Wasp.Project.PerService as PerService

data BuildStartArgs = BuildStartArgs
  { envVarInputs :: PerService [EnvVarInput],
    ports :: PerService (Maybe PortNumber)
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> envVarInputsParser
    <*> appPortsParser
  where
    envVarInputsParser = for PerService.names $ \name ->
      liftA2
        (<>)
        (Opt.many $ makeEnvironmentVariableParser name (name ++ "-env") (head name))
        (Opt.many $ makeEnvironmentFileParser name (name ++ "-env-file"))

    makeEnvironmentVariableParser :: String -> String -> Char -> Opt.Parser EnvVarInput
    makeEnvironmentVariableParser targetName longOptionName shortOptionName =
      FromFlag ("--" ++ longOptionName)
        <$> Opt.option
          envVarReader
          ( Opt.long longOptionName
              <> Opt.short shortOptionName
              <> Opt.metavar "NAME=VALUE"
              <> Opt.help ("Set an environment variable for the " <> targetName <> " (can be used multiple times)")
          )

    makeEnvironmentFileParser :: String -> String -> Opt.Parser EnvVarInput
    makeEnvironmentFileParser targetName longOptionName =
      FromFileArgument
        <$> Opt.option
          filePathReader
          ( Opt.long longOptionName
              <> Opt.metavar "FILE_PATH"
              <> Opt.help ("Load environment variables for the " <> targetName <> " from a file (can be used multiple times)")
              <> Opt.action "file"
          )
