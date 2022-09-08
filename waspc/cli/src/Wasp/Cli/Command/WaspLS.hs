module Wasp.Cli.Command.WaspLS
  ( runWaspLS,
  )
where

import qualified Options.Applicative as O
import qualified Wasp.LSP.Server as LS

runWaspLS :: IO ()
runWaspLS = do
  args <- parseArgsOrPrintUsageAndExit
  LS.serve $ optionsLogFile $ options args

parseArgsOrPrintUsageAndExit :: IO Args
parseArgsOrPrintUsageAndExit =
  O.execParser $
    O.info
      (O.helper <*> parseArgs)
      (O.progDesc "LSP Server for the Wasp language" <> O.fullDesc)

data Args = Args
  { options :: Options
  }

data Options = Options
  { optionsLogFile :: Maybe FilePath,
    optionsUseStdio :: Bool
  }

-- NOTE: Here we assume that first arg on command line is "waspls".
parseArgs :: O.Parser Args
parseArgs = Args <$> O.hsubparser (O.command "waspls" (O.info parseOptions (O.progDesc "Run Wasp Language Server")))

parseOptions :: O.Parser Options
parseOptions = Options <$> O.optional parseLogFile <*> parseStdio
  where
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
