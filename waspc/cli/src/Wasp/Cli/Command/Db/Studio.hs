module Wasp.Cli.Command.Db.Studio
  ( studio,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, WaspRootRequirement (WaspRootRequirement), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg

studio :: Command ()
studio = do
  WaspRootRequirement waspProjectDir <- require
  let genProjectDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Running studio..."

  chan <- liftIO newChan
  _ <- liftIO $ readJobMessagesAndPrintThemPrefixed chan `concurrently` runStudio genProjectDir chan

  error "This should never happen, studio should never stop."
