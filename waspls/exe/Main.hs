module Main where

import Data.Version (showVersion)
import Options.Applicative
import qualified Paths_waspls
import Wasp.LSP.Server

main :: IO ()
main = do
  options <- execParser parse
  if version options
    then printVersion
    else case mode options of
      Version -> printVersion
      Run -> run $ logFile options
  where
    printVersion = putStrLn $ showVersion Paths_waspls.version

parse :: ParserInfo Options
parse =
  info
    (helper <*> parseOptions)
    (progDesc "LSP Server for the Wasp language" <> fullDesc)

data Mode = Run | Version

data Options = Options
  { mode :: Mode,
    version :: Bool,
    logFile :: Maybe FilePath,
    useStdio :: Bool
  }

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> parseVersion <*> optional parseLogFile <*> parseStdio
  where
    parseLogFile =
      strOption
        ( long "log"
            <> help "Write log output to this file, if present. If not present, no logs are written. If set to `[OUTPUT]`, log output is sent to the LSP client."
            <> action "file"
            <> metavar "LOG_FILE"
        )

    parseVersion =
      switch
        ( long "version"
            <> short 'v'
            <> help "Display version and exit"
        )

    -- vscode passes this option to the language server. waspls always uses stdio,
    -- so this switch is ignored.
    parseStdio =
      switch
        ( long "stdio"
            <> help "Use stdio (this flag is ignored, stdio is always used)"
        )

    parseMode = hsubparser versionCommand <|> pure Run

    versionCommand =
      command
        "version"
        (info (pure Version) (fullDesc <> progDesc "Display version and exit"))
        <> metavar "version"
