module Wasp.Cli.Command.Db.Reset
  ( reset,
    ResetArgs (..),
    resetArgsParser,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Db (makeDbCommand)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Util.Parser (ArgsParser (..))
import Wasp.Generator.DbGenerator.Common (ResetArgs (..))
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady))
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)

reset :: ResetArgs -> Command ()
reset resetArgs = makeDbCommand [PrismaCliReady] $ \_appSpec projectRunConfig -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Resetting the database..."
  liftIO (dbReset (ServerGenerator.makeEnvVars projectRunConfig) genProjectDir resetArgs) >>= \case
    Left errorMsg -> throwError $ CommandError "Database reset failed" errorMsg
    Right () -> cliSendMessageC $ Msg.Success "Database reset successfully!"

resetArgsParser :: ArgsParser ResetArgs
resetArgsParser =
  ArgsParser "wasp db reset" $
    ResetArgs
      <$> Opt.switch
        ( Opt.long "force"
            <> Opt.help "Skip the confirmation prompt"
        )
