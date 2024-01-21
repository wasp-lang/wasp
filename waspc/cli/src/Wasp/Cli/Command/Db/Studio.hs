module Wasp.Cli.Command.Db.Studio
  ( studio,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

studio :: Command ()
studio = do
  InWaspProject waspProjectDir <- require
  let genProjectDir =
        waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Running studio..."

  chan <- liftIO newChan
  _ <- liftIO $ readJobMessagesAndPrintThemPrefixed chan `concurrently` runStudio genProjectDir chan

  error "This should never happen, studio should never stop."
