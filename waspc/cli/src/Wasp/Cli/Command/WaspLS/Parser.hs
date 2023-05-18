module Wasp.Cli.Command.WaspLS.Parser (waspls) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    action,
    help,
    long,
    metavar,
    optional,
    strOption,
    switch,
  )
import Wasp.Cli.Command.Call (Call (WaspLS), WaspLSArgs (WaspLSArgs))
import Wasp.Cli.Parser.Util (mkCommand)

waspls :: Mod CommandFields Call
waspls = mkCommand "waspls" parseWaspLS "Run Wasp Language Server. Add --help to get more info."

parseWaspLS :: Parser Call
parseWaspLS = WaspLS <$> parseWaspLSArgs
  where
    parseWaspLSArgs = WaspLSArgs <$> optional parseLogFile <*> parseStdio
    parseLogFile =
      strOption
        ( long "log"
            <> help "Write log output to this file, if present. If not present, no logs are written. If set to `[OUTPUT]`, log output is sent to the LSP client."
            <> action "file"
            <> metavar "LOG_FILE"
        )

    -- vscode passes this option to the language server. waspls always uses stdio,
    -- so this switch is ignored.
    parseStdio =
      switch
        ( long "stdio"
            <> help "Use stdio for communicating with LSP client. This is the only communication method we support for now, so this is the default anyway and this flag has no effect."
        )
