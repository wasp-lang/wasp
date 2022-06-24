module Main where

import Data.Version (showVersion)
import qualified Options.Applicative as O
import qualified Paths_waspls
import qualified Wasp.LSP.Server as LSP

main :: IO ()
main = do
  args <- parseArgsOrPrintUsageAndExit
  case command args of
    PrintVersion -> printVersion
    Serve -> LSP.serve $ optionsLogFile $ options args
  where
    printVersion = putStrLn $ showVersion Paths_waspls.version

parseArgsOrPrintUsageAndExit :: IO Args
parseArgsOrPrintUsageAndExit =
  O.execParser $
    O.info
      (O.helper <*> parseArgs)
      (O.progDesc "LSP Server for the Wasp language" <> O.fullDesc)

data Args = Args
  { command :: Command,
    options :: Options
  }

data Command = PrintVersion | Serve

data Options = Options
  { optionsLogFile :: Maybe FilePath,
    optionsUseStdio :: Bool
  }

parseArgs :: O.Parser Args
parseArgs = Args <$> parseCommand <*> parseOptions

parseCommand :: O.Parser Command
parseCommand = O.hsubparser versionCommand O.<|> pure Serve
  where
    versionCommand =
      O.command
        "version"
        (O.info (pure PrintVersion) (O.fullDesc <> O.progDesc "Display version and exit"))
        <> O.metavar "version"

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
