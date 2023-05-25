module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Main.Utf8 (withUtf8)
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Build (build)
import qualified Wasp.Cli.Command.Call as Command
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import Wasp.Cli.Command.Db.Cli (dbCli)
import Wasp.Cli.Command.Deploy (deploy)
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.ShellCompletion (completion)
import Wasp.Cli.Command.Start (start)
import Wasp.Cli.Command.Telemetry as Telemetry
  ( considerSendingData,
    telemetry,
  )
import Wasp.Cli.Command.Test (test)
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Cli.Parser (parseCliArgs)
import Wasp.Util (indent)
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  commandCall <- parseCliArgs
  telemetryThread <- Async.async $ runCommand $ considerSendingData commandCall
  run commandCall

  -- If sending of telemetry data is still not done 1 second since commmand finished, abort it.
  -- We also make sure here to catch all errors that might get thrown and silence them.
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = putStrLn $ "\nInternal Wasp error (bug in compiler):\n" ++ indent 2 (show e)

run :: Command.Call -> IO ()
run = \case
  Command.New args -> runCommand $ createNewProject args
  Command.Start arg -> runCommand $ start arg
  Command.Clean -> runCommand clean
  Command.Uninstall -> runCommand uninstall
  Command.Build -> runCommand build
  -- This command is called by wasp new, internally.
  Command.Compile -> runCommand compile
  Command.Db args -> dbCli args
  Command.Version -> printVersion
  Command.Telemetry -> runCommand telemetry
  Command.Deps -> runCommand deps
  Command.Dockerfile -> runCommand printDockerfile
  Command.Info -> runCommand info
  Command.Completion args -> runCommand $ completion args
  Command.WaspLS args -> runCommand $ runWaspLS args
  Command.Deploy args -> runCommand $ deploy args
  Command.Test args -> runCommand $ test args

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
