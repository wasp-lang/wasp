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
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Generator
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

test :: [String] -> Command ()
test [] = throwError $ CommandError "Not enough arguments" "Expected: wasp test client <args>"
test ("client" : args) = watchAndTest $ Wasp.Generator.testWebApp args
test ("server" : _args) = throwError $ CommandError "Invalid arguments" "Server testing not yet implemented."
test _ = throwError $ CommandError "Invalid arguments" "Expected: wasp test client <args>"

watchAndTest :: (Path' Abs (Dir ProjectRootDir) -> IO (Either String ())) -> Command ()
watchAndTest testRunner = do
  InWaspProject waspRoot <- require
  let outDir = waspRoot </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir

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
