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
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (TestArgs (TestClient))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Generator
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Message as Msg

test :: TestArgs -> Command ()
test (TestClient args) = watchAndTest $ Wasp.Generator.testWebApp args

watchAndTest :: (Path' Abs (Dir ProjectRootDir) -> IO (Either String ())) -> Command ()
watchAndTest testRunner = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  warnings <- compile

  cliSendMessageC $ Msg.Start "Watching for file changes and running tests ..."

  watchOrStartResult <- liftIO $ do
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar
    watchWaspProjectSource `race` testRunner outDir

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left testError -> throwError $ CommandError "Test failed" testError
      Right () -> return ()
