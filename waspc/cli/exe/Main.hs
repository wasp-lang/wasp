module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Main.Utf8 (withUtf8)
import Options.Applicative (Applicative (liftA2), Parser, (<**>), (<|>))
import qualified Options.Applicative as O
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.Call (Call (..))
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Start.Db as Command.Start.Db
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Util (indent)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  commandCall <- runParser
  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall
  run commandCall
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = putStrLn $ "\nInternal Wasp error (bug in compiler):\n" ++ indent 2 (show e)

run :: Call -> IO ()
run = \case
  (New _) -> pure ()
  Start -> runCommand start
  StartDb -> runCommand Command.Start.Db.start
  Clean -> runCommand clean
  Uninstall -> runCommand uninstall
  Compile -> runCommand compile
  Db _ -> pure ()
  Build -> runCommand build
  Version -> printVersion
  Telemetry -> runCommand Telemetry.telemetry
  Deps -> runCommand deps
  Dockerfile -> runCommand printDockerfile
  Info -> runCommand info
  WaspLS -> runWaspLS
  Deploy _ -> pure ()
  Test _ -> pure ()

runParser :: IO Call
runParser = do O.customExecParser p opts
  where
    opts =
      O.info
        (parserSuite <**> O.helper)
        ( O.fullDesc
            <> O.footer
              ( unlines
                  [ Term.applyStyles [Term.Green] "Docs:" <> " https://wasp-lang.dev/docs",
                    Term.applyStyles [Term.Magenta] "Discord (chat):" <> " https://discord.gg/rzdnErX",
                    Term.applyStyles [Term.Cyan] "Newsletter:" <> " https://wasp-lang.dev/#signup"
                  ]
              )
        )
    p = O.prefs O.showHelpOnEmpty

parserSuite :: Parser Call
parserSuite =
  O.subparser
    ( O.command "version" (O.info (O.helper <*> pure Version) (O.progDesc "Prints current version of CLI."))
        <> O.command "waspls" (O.info (O.helper <*> pure WaspLS) (O.progDesc "Run Wasp Language Server. Add --help to get more info."))
        <> O.command "uninstall" (O.info (O.helper <*> pure Uninstall) (O.progDesc "Removes Wasp from your system."))
        <> O.commandGroup "GENERAL"
    )
    <|> O.subparser
      ( O.command "start" (O.info (O.helper <*> pure Start) (O.progDesc "Runs Wasp app in development mode, watching for file changes."))
          <> O.command "start db" (O.info (O.helper <*> pure Command.Call.StartDb) (O.progDesc "Starts managed development database for you."))
          <> O.commandGroup "IN PROJECT"
      )

printVersion :: IO ()
printVersion = do
  putStrLn $
    unlines
      [ show waspVersion,
        "",
        "If you wish to install/switch to the latest version of Wasp, do:",
        "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s",
        "",
        "If you want specific x.y.z version of Wasp, do:",
        "  curl -sSL https://get.wasp-lang.dev/installer.sh | sh -s -- -v x.y.z",
        "",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
      ]
