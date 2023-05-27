module Wasp.Cli.Command.WaspLS.Parser (waspls) where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
  )
import qualified Options.Applicative as O
import Wasp.Cli.Command.Call (Call (WaspLS), WaspLSArgs (WaspLSArgs))
import Wasp.Cli.Parser.Util (mkNormalCommand)

waspls :: Mod CommandFields Call
waspls =
  mkNormalCommand
    "waspls"
    "Run Wasp Language Server. Add --help to get more info."
    parseWaspLS

parseWaspLS :: Parser Call
parseWaspLS = WaspLS <$> parseWaspLSArgs
  where
    parseWaspLSArgs = WaspLSArgs <$> O.optional parseLogFile <*> parseStdio
    parseLogFile =
      O.strOption
        ( O.long "log"
            <> O.help "Write log output to this file, if present. If not present, no logs are written. If set to `[OUTPUT]`, log output is sent to the LSP client."
            <> O.action "file"
            <> O.metavar "LOG_FILE"
        )

    -- vscode passes this option to the language server. waspls always uses stdio,
    -- so this switch is ignored.
    parseStdio =
      O.switch
        ( O.long "stdio"
            <> O.help "Use stdio for communicating with LSP client. This is the only communication method we support for now, so this is the default anyway and this flag has no effect."
        )
