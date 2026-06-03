module Wasp.Cli.Command.WaspLS
  ( parserInfo,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Command.Definition (CommandParserInfo, leafWithArgs, runWaspIO)
import qualified Wasp.LSP.Server as LS

parserInfo :: CommandParserInfo
parserInfo =
  leafWithArgs
    "Run the Wasp Language Server."
    (serve <$> logFileParser <*> stdioParser)
  where
    -- vscode passes --stdio, but we always use stdio, so the flag is ignored.
    serve logFile _useStdio = runWaspIO (LS.serve logFile)

    logFileParser =
      Opt.optional $
        Opt.strOption $
          Opt.long "log"
            <> Opt.help "Write log output to this file. Set to `[OUTPUT]` to send it to the LSP client."
            <> Opt.action "file"
            <> Opt.metavar "LOG_FILE"

    stdioParser =
      Opt.switch
        ( Opt.long "stdio"
            <> Opt.help "Use stdio for communicating with the LSP client. This is the only supported transport and is always on; the flag is accepted for compatibility."
        )
