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
import Wasp.Cli.AppComponentPorts (findAppComponentPorts)
import Wasp.Cli.AppComponentUrls (makeDevUrls)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Cli.RunConfigs (makeRunConfigs)
import qualified Wasp.Generator
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( WaspProjectDir,
    generatedAppDirInWaspProjectDir,
  )

test :: [String] -> Command ()
test [] = throwError $ CommandError "Not enough arguments" "Expected: wasp test client <args>"
test ("client" : args) = watchAndTest $ \clientRunConfig ->
  Wasp.Generator.testWebApp clientRunConfig args
test ("server" : _args) = throwError $ CommandError "Invalid arguments" "Server testing not yet implemented."
test _ = throwError $ CommandError "Invalid arguments" "Expected: wasp test client <args>"

watchAndTest :: (WebAppRunConfig -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())) -> Command ()
watchAndTest testRunner = withProjectLock $ do
  InWaspProject waspRoot <- require
  let outDir = waspRoot </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  (warnings, appSpec) <- compile

  ports <- findAppComponentPorts (Nothing, Nothing)
  let (clientRunConfig, _) = makeRunConfigs $ makeDevUrls appSpec ports

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
