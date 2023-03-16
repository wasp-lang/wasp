module Wasp.Cli.Command.Test
  ( test,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib as Lib
import qualified Wasp.Message as Msg

test :: [String] -> Command ()
test [] = throwError $ CommandError "Not enough arguments." "Expected: wasp test client <args>"
test ("client" : args) = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  warnings <- compile

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Running client tests..."

  watchOrStartResult <- liftIO $ do
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspRoot outDir ongoingCompilationResultMVar
    let testWebApp = Lib.testWebApp args outDir
    watchWaspProjectSource `race` testWebApp

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left testError -> throwError $ CommandError "Test failed" testError
      Right () -> return ()
test _ = throwError $ CommandError "Invalid arguments." "Expected: wasp test client <args>"
