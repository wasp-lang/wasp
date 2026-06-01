module Wasp.Cli.Command.Start.ArgumentsParser
  ( startArgsParser,
    StartArgs (..),
  )
where

import qualified Options.Applicative as Opt

data StartArgs = StartArgs
  { disableLanExposure :: Bool,
    hostOverride :: Maybe String
  }

startArgsParser :: Opt.Parser StartArgs
startArgsParser =
  StartArgs
    <$> Opt.switch
      ( Opt.long "no-lan"
          <> Opt.help "Do not expose the app on the local network. URLs default to localhost."
      )
    <*> Opt.optional
      ( Opt.strOption $
          Opt.long "host"
            <> Opt.metavar "HOSTNAME"
            <> Opt.help
              ( "Hostname (or IP address) used to compute the URLs the dev app is reachable at. "
                  <> "A bare IP gets a `.nip.io` suffix appended so OAuth-style integrations keep working. "
                  <> "Defaults to the local network IP."
              )
      )
