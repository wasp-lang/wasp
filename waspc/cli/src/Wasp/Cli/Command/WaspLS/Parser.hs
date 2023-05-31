module Wasp.Cli.Command.WaspLS.Parser (waspLSParser) where

import Options.Applicative
  ( Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (CommandCall (WaspLS), WaspLSArgs (WaspLSArgs))

waspLSParser :: Parser CommandCall
waspLSParser = WaspLS <$> waspLSArgsParser
  where
    waspLSArgsParser = WaspLSArgs <$> O.optional logFileParser <*> stdioParser
    logFileParser =
      O.strOption
        ( O.long "log"
            <> O.help "Write log output to this file, if present. If not present, no logs are written. If set to `[OUTPUT]`, log output is sent to the LSP client."
            <> O.action "file"
            <> O.metavar "LOG_FILE"
        )

    -- vscode passes this option to the language server. waspls always uses stdio,
    -- so this switch is ignored.
    stdioParser =
      O.switch
        ( O.long "stdio"
            <> O.help "Use stdio for communicating with LSP client. This is the only communication method we support for now, so this is the default anyway and this flag has no effect."
        )
