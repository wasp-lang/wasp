module Wasp.Cli.Command.BuildStart.ArgumentsParser
  ( buildStartArgsParser,
    BuildStartArgs (..),
  )
where

import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import Wasp.Cli.AppComponentUrls (defaultDevClientPort, defaultDevServerPort)
import Wasp.Cli.Util.EnvVarArgument (EnvVarArgument, envVarArgumentFileParser, envVarArgumentLiteralParser)
import Wasp.Cli.Util.PortArgument (portOption)

data BuildStartArgs = BuildStartArgs
  { clientPort :: PortNumber,
    serverPort :: PortNumber,
    clientEnvVars :: [EnvVarArgument],
    serverEnvVars :: [EnvVarArgument]
  }

buildStartArgsParser :: Opt.Parser BuildStartArgs
buildStartArgsParser =
  BuildStartArgs
    <$> portParserForComponent "client" defaultDevClientPort
    <*> portParserForComponent "server" defaultDevServerPort
    <*> environmentVariableParsersForComponent "client"
    <*> environmentVariableParsersForComponent "server"
  where
    portParserForComponent name =
      portOption
        (name ++ "-port")
        ("Port to run the " ++ name ++ " on")

    environmentVariableParsersForComponent name =
      liftA2
        (<>)
        (envVarInlinesParserForComponent name)
        (envVarFilesParserForComponent name)

    envVarInlinesParserForComponent name =
      Opt.many $
        envVarArgumentLiteralParser
          (head name) -- e.g. "-c"
          (name ++ "-env") -- e.g. "--client-env"
          ("Set an environment variable for the " ++ name ++ " (can be used multiple times)")

    envVarFilesParserForComponent name =
      Opt.many $
        envVarArgumentFileParser
          (name ++ "-env-file") -- e.g. "--client-env-file"
          ("Load environment variables for the " ++ name ++ " from a file (can be used multiple times)")
