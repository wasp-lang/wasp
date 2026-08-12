module Wasp.Cli.Command.Test
  ( test,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, (</>))
import StrongPath.Types (Path')
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.ProjectLock (withProjectLock)
import qualified Wasp.Generator
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Generator.WebAppGenerator.RunConfig (ClientRunConfig, makeClientRunConfig)
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( WaspProjectDir,
    generatedAppDirInWaspProjectDir,
  )
import qualified Wasp.Util.AppLocation as AL

test :: [String] -> Command ()
test [] = throwError $ CommandError "Not enough arguments" "Expected: wasp test client <args>"
test ("client" : args) = watchAndTest $ \clientRunConfig ->
  Wasp.Generator.testWebApp clientRunConfig args
test ("server" : _args) = throwError $ CommandError "Invalid arguments" "Server testing not yet implemented."
test _ = throwError $ CommandError "Invalid arguments" "Expected: wasp test client <args>"

watchAndTest :: (ClientRunConfig -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())) -> Command ()
watchAndTest testRunner = withProjectLock $ do
  InWaspProject waspRoot <- require
  let outDir = waspRoot </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  (warnings, appSpec) <- compile
  let clientRunConfig = defaultDevClientRunConfig appSpec

  cliSendMessageC $ Msg.Start "Watching for file changes and running tests ..."

  watchOrStartResult <- liftIO $ do
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar

    -- Vitest must run from the root of the project because Vite won't resolve
    -- files outside of the project root (in this case, user src/ dir which the
    -- web app imports).
    watchWaspProjectSource `race` testRunner clientRunConfig waspRoot

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left testError -> throwError $ CommandError "Test failed" testError
      Right () -> return ()
  where
    -- The test runner never binds a port and never talks to a server, but the
    -- client still validates Wasp's env vars, so we give it the development
    -- defaults.
    defaultDevClientRunConfig :: AS.AppSpec -> ClientRunConfig
    defaultDevClientRunConfig appSpec =
      makeClientRunConfig
        (WebApp.makeDefaultDevClientLocation appSpec)
        (AL.url Server.defaultDevServerLocation)
