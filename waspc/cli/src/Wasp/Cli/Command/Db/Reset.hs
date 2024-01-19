module Wasp.Cli.Command.Db.Reset
  ( reset,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

reset :: Command ()
reset = do
  InWaspProject waspProjectDir <- require
  let genProjectDir =
        waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Resetting the database..."

  liftIO (dbReset genProjectDir) >>= \case
    Left errorMsg -> cliSendMessageC $ Msg.Failure "Database reset failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database reset successfully!"
