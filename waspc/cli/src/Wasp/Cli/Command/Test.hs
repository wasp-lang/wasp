module Wasp.Cli.Command.Test
  ( test,
    parserInfo,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir, (</>))
import StrongPath.Types (Path')
import Wasp.Cli.Command (Command, CommandError (..), runCommand)
import qualified Wasp.Cli.Command.Call as Call
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Telemetry (runWithTelemetry)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( WaspProjectDir,
    generatedAppDirInWaspProjectDir,
  )

data TestArgs
  = TestClient [String]
  | TestServer [String]

parserInfo :: Opt.ParserInfo (IO ())
parserInfo =
  Opt.info testCommands (Opt.progDesc "Run tests in your project.")
  where
    testCommands =
      Opt.hsubparser $
        Opt.command "client" (Opt.info (run . TestClient <$> passthroughArgsParser) (Opt.progDesc "Run client-side tests via Vitest"))
          <> Opt.command "server" (Opt.info (run . TestServer <$> passthroughArgsParser) (Opt.progDesc "Run server-side tests (not yet implemented)"))

    run = runWithTelemetry Call.Other . runCommand . test

    passthroughArgsParser :: Opt.Parser [String]
    passthroughArgsParser =
      Opt.many $
        Opt.strArgument $
          Opt.metavar "TEST_RUNNER_ARGS..."
            <> Opt.help "Arguments passed through to the test runner"

test :: TestArgs -> Command ()
test (TestClient args) = watchAndTest $ Wasp.Generator.testWebApp args
test (TestServer _args) = throwError $ CommandError "Invalid arguments" "Server testing not yet implemented."

watchAndTest :: (Path' Abs (Dir WaspProjectDir) -> IO (Either String ())) -> Command ()
watchAndTest testRunner = do
  InWaspProject waspRoot <- require
  let outDir = waspRoot </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  warnings <- compile

  cliSendMessageC $ Msg.Start "Watching for file changes and running tests ..."

  watchOrStartResult <- liftIO $ do
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar

    -- Vitest must run from the root of the project because Vite won't resolve
    -- files outside of the project root (in this case, user src/ dir which the
    -- web app imports).
    watchWaspProjectSource `race` testRunner waspRoot

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left testError -> throwError $ CommandError "Test failed" testError
      Right () -> return ()
