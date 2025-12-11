module Wasp.Cli.Command.Db.Reset
  ( reset,
    ResetArgs (..),
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Generator.DbGenerator.Common (ResetArgs (..))
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

reset :: Arguments -> Command ()
reset = withArguments "wasp db reset" resetArgsParser $ \resetArgs -> do
  InWaspProject waspProjectDir <- require
  let genProjectDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedCodeDirInDotWaspDir

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
