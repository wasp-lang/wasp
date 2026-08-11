module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.Util.EnvVarArgument (envVarReader)
import Wasp.Cli.Util.EnvVarInputs (EnvVarInput (..))
import Wasp.Cli.Util.PathArgument (filePathReader)
import Wasp.Cli.Util.PortArgument (portOption)
import qualified Wasp.Generator.Client as Client
import qualified Wasp.Generator.Server as Server

data BuildStartArgs = BuildStartArgs
  { clientEnvVarInputs :: [EnvVarInput],
    serverEnvVarInputs :: [EnvVarInput],
    clientPort :: PortNumber,
    serverPort :: PortNumber
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> makeEnvVarInputsParser "client" 'c'
    <*> makeEnvVarInputsParser "server" 's'
    <*> portOption "client-port" "Port to run the client on" Client.defaultPort
    <*> portOption "server-port" "Port to run the server on" Server.defaultPort
  where
    makeEnvVarInputsParser :: String -> Char -> Opt.Parser [EnvVarInput]
    makeEnvVarInputsParser targetName shortOptionName =
      liftA2
        (<>)
        (Opt.many $ makeEnvironmentVariableParser targetName (targetName ++ "-env") shortOptionName)
        (Opt.many $ makeEnvironmentFileParser targetName (targetName ++ "-env-file"))

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
