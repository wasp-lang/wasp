module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as O
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.Call (Call (..))
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
import Wasp.Cli.Parser (parserRunnerSettings)
import Wasp.Util (indent)
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  commandCall <- runParser
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

run :: Call -> IO ()
run = \case
  (New args) -> runCommand $ createNewProject args
  Start arg -> runCommand $ start arg
  Clean -> runCommand clean
  Uninstall -> runCommand uninstall
  Build -> runCommand build
  -- This command is called by wasp new, internally.
  Compile -> runCommand compile
  Db args -> dbCli args
  Version -> printVersion
  Telemetry -> runCommand telemetry
  Deps -> runCommand deps
  Dockerfile -> runCommand printDockerfile
  Info -> runCommand info
  Completion args -> runCommand $ completion args
  WaspLS args -> runCommand $ runWaspLS args
  Deploy args -> runCommand $ deploy args
  Test args -> runCommand $ test args

runParser :: IO Call
runParser = uncurry O.customExecParser parserRunnerSettings

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
