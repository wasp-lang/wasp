module Wasp.Cli.Command.Db.Reset
  ( reset,
    resetArgsParser,
    ResetArgs (..),
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Generator.DbGenerator.Common (ResetArgs (..))
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)

reset :: ResetArgs -> Command ()
reset resetArgs = do
  InWaspProject waspProjectDir <- require
  let genProjectDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Resetting the database..."
  liftIO (dbReset genProjectDir resetArgs) >>= \case
    Left errorMsg -> throwError $ CommandError "Database reset failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database reset successfully!"

resetArgsParser :: Opt.Parser ResetArgs
resetArgsParser =
  ResetArgs
    <$> Opt.switch
      ( Opt.long "force"
          <> Opt.help "Skip the confirmation prompt"
      )
