module Wasp.Cli.Command.WaspLS
  ( runWaspLS,
    WaspLSArgs (..),
    waspLSArgsParser,
    parserInfo,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Command.Definition (CommandParserInfo, leafWithArgs, runWaspIO)
import qualified Wasp.LSP.Server as LS

data WaspLSArgs = WaspLSArgs
  { waspLSLogFile :: Maybe FilePath,
    -- vscode passes --stdio; we always use stdio so the value is ignored.
    waspLSUseStdio :: Bool
  }

parserInfo :: CommandParserInfo
parserInfo =
  leafWithArgs
    "Run the Wasp Language Server."
    waspLSArgsParser
    (runWaspIO . runWaspLS)

runWaspLS :: WaspLSArgs -> IO ()
runWaspLS args = LS.serve (waspLSLogFile args)

waspLSArgsParser :: Opt.Parser WaspLSArgs
waspLSArgsParser =
  WaspLSArgs
    <$> Opt.optional
      ( Opt.strOption $
          Opt.long "log"
            <> Opt.help "Write log output to this file. Set to `[OUTPUT]` to send it to the LSP client."
            <> Opt.action "file"
            <> Opt.metavar "LOG_FILE"
      )
    <*> Opt.switch
      ( Opt.long "stdio"
          <> Opt.help "Use stdio for communicating with the LSP client. This is the only supported transport and is always on; the flag is accepted for compatibility."
      )
