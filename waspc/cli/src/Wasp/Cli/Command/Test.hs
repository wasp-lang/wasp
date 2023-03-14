module Wasp.Cli.Command.Test
  ( test,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib as Lib
import qualified Wasp.Message as Msg

test :: Command ()
test = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Running tests..."
  testResult <- liftIO $ Lib.test outDir
  case testResult of
    Left testError -> throwError $ CommandError "Test failed" testError
    Right () -> return ()
