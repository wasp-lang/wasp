module Main where

import Data.Version (showVersion)
import qualified Options.Applicative as O
import qualified Paths_waspls
import qualified Wasp.LSP.Server as LSP

main :: IO ()
main = do
  args <- O.execParser parseArgsWithHelp
  case command args of
    PrintVersion -> doPrintVersion
    Serve ->
      if printVersion (options args)
        then doPrintVersion
        else LSP.run $ logFile $ options args
  where
    doPrintVersion = putStrLn $ showVersion Paths_waspls.version

parseArgsWithHelp :: O.ParserInfo Args
parseArgsWithHelp =
  O.info
    (O.helper <*> parseArgs)
    (O.progDesc "LSP Server for the Wasp language" <> O.fullDesc)

data Args = Args
  { command :: Command,
    options :: Options
  }

data Command = PrintVersion | Serve

data Options = Options
  { printVersion :: Bool,
    logFile :: Maybe FilePath,
    useStdio :: Bool
  }

parseArgs :: O.Parser Args
parseArgs = Args <$> parseCommand <*> parseOptions
  where
    parseCommand = O.hsubparser versionCommand O.<|> pure Serve

    versionCommand =
      O.command
        "version"
        (O.info (pure PrintVersion) (O.fullDesc <> O.progDesc "Display version and exit"))
        <> O.metavar "version"

parseOptions :: O.Parser Options
parseOptions = Options <$> parseVersionFlag <*> O.optional parseLogFile <*> parseStdio
  where
    parseLogFile =
      O.strOption
        ( O.long "log"
            <> O.help "Write log output to this file, if present. If not present, no logs are written. If set to `[OUTPUT]`, log output is sent to the LSP client."
            <> O.action "file"
            <> O.metavar "LOG_FILE"
        )

    parseVersionFlag =
      O.switch
        ( O.long "version"
            <> O.short 'v'
            <> O.help "Display version and exit"
        )

    -- vscode passes this option to the language server. waspls always uses stdio,
    -- so this switch is ignored.
    parseStdio =
      O.switch
        ( O.long "stdio"
            <> O.help "Use stdio protocol for LSP communication. This is the only supported protocol, so stdio is used by default and this flag does nothing."
        )
