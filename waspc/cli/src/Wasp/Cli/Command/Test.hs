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
import Wasp.Cli.AppComponents (makeDevRunConfigs)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.LockedProject (withLockedProject)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Generator
import qualified Wasp.Generator.Client as Client
import qualified Wasp.Generator.Server as Server
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( WaspProjectDir,
    generatedAppDirInWaspProjectDir,
  )

test :: [String] -> Command ()
test [] = throwError $ CommandError "Not enough arguments" "Expected: wasp test client <args>"
test ("client" : args) = watchAndTest $ \appSpec ->
  let (client, _) = makeDevRunConfigs appSpec Client.defaultPort Server.defaultPort
   in Wasp.Generator.testWebApp (Client.devEnvVars client) args
test ("server" : _args) = throwError $ CommandError "Invalid arguments" "Server testing not yet implemented."
test _ = throwError $ CommandError "Invalid arguments" "Expected: wasp test client <args>"

watchAndTest :: (AS.AppSpec -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())) -> Command ()
watchAndTest makeTestRunner = withLockedProject $ do
  InWaspProject waspRoot <- require
  let outDir = waspRoot </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  (warnings, appSpec) <- compile

  cliSendMessageC $ Msg.Start "Watching for file changes and running tests ..."

  watchOrStartResult <- liftIO $ do
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar

    -- Vitest must run from the root of the project because Vite won't resolve
    -- files outside of the project root (in this case, user src/ dir which the
    -- web app imports).
    watchWaspProjectSource `race` makeTestRunner appSpec waspRoot

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left testError -> throwError $ CommandError "Test failed" testError
      Right () -> return ()
