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
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Definition (CommandParserInfo, commandGroup, commandWithArgs)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
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

parserInfo :: CommandParserInfo
parserInfo =
  commandGroup
    "Run tests in your project."
    [ ("client", commandWithArgs "Run client-side tests via Vitest" (test . TestClient <$> passthroughArgsParser)),
      ("server", commandWithArgs "Run server-side tests (not yet implemented)" (test . TestServer <$> passthroughArgsParser))
    ]
  where
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
