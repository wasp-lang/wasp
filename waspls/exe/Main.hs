module Main where

import Data.Version (showVersion)
import Options.Applicative
import qualified Paths_waspls
import Wasp.LSP.Server

data Mode = Run | Version

data Options = Options
  { mode :: Mode,
    version :: Bool,
    logFile :: Maybe FilePath
  }

parseOptions :: Parser Options
parseOptions = Options <$> parseMode <*> parseVersion <*> optional parseLogFile
  where
    parseLogFile =
      strOption
        ( long "log"
            <> help "Write log output to this file, if present"
            <> action "file"
            <> metavar "LOG_FILE"
        )

    parseVersion =
      switch
        ( long "version"
            <> short 'v'
            <> help "Display version"
        )

    parseMode = hsubparser versionCommand <|> pure Run

    versionCommand =
      command
        "version"
        (info (pure Version) (fullDesc <> progDesc "Display version"))
        <> metavar "version"

parseInfo :: ParserInfo Options
parseInfo =
  info
    (helper <*> parseOptions)
    (progDesc "LSP Server for the Wasp language" <> fullDesc)

main :: IO ()
main = do
  options <- execParser parseInfo
  if version options
    then printVersion
    else case mode options of
      Version -> printVersion
      Run -> run $ logFile options
  where
    printVersion = putStrLn $ showVersion Paths_waspls.version
