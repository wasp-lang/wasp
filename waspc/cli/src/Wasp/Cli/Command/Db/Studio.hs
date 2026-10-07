module Wasp.Cli.Command.Db.Studio
  ( studio,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

studio :: Command ()
studio = do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Running studio..."

  _ <- liftIO $ Job.run $ Job.prefixWith Job.Db $ runStudio genProjectDir

  error "This should never happen, studio should never stop."
