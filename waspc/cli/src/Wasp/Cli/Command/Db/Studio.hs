module Wasp.Cli.Command.Db.Studio
  ( studio,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

studio :: Command ()
studio = do
  InWaspProject waspProjectDir <- require
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Running studio..."

  chan <- liftIO newChan
  _ <- liftIO $ Output.printEventsPrefixedUntilExit chan `concurrently` Job.runJob (runStudio genProjectDir) chan

  error "This should never happen, studio should never stop."
