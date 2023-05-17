module Main where

import Control.Applicative ((<**>))
import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Main.Utf8 (withUtf8)
import qualified Options.Applicative as O
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.Call (Call (..), DbArgs (..))
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Db (runDbCommand)
import qualified Wasp.Cli.Command.Db.Migrate as Command.Db.Migrate
import qualified Wasp.Cli.Command.Db.Reset as Command.Db.Reset
import qualified Wasp.Cli.Command.Db.Seed as Command.Db.Seed
import qualified Wasp.Cli.Command.Db.Studio as Command.Db.Studio
import Wasp.Cli.Command.Deploy (deploy)
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Start.Db as Command.Start.Db
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Test (test)
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Cli.Parser (parserRunnerSettings)
import Wasp.Util (indent)
import qualified Wasp.Util.Terminal as Term
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  commandCall <- runParser
  telemetryThread <- Async.async $ runCommand $ Telemetry.considerSendingData commandCall
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
  (New args) -> undefined
  Start arg -> runCommand $ start arg
  Clean -> runCommand clean
  Uninstall -> runCommand uninstall
  -- This command is called by wasp new, internally.
  Compile -> runCommand compile
  Db args ->
    case args of
      DbStart -> runCommand Command.Start.Db.start
      DbMigrateDev migrateArgs -> undefined
      DbReset -> runDbCommand Command.Db.Reset.reset
      DbSeed seedName -> runDbCommand $ Command.Db.Seed.seed seedName
      DbStudio -> runDbCommand Command.Db.Studio.studio
  Build -> runCommand build
  Version -> printVersion
  Telemetry -> runCommand Telemetry.telemetry
  Deps -> runCommand deps
  Dockerfile -> runCommand printDockerfile
  Info -> runCommand info
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
