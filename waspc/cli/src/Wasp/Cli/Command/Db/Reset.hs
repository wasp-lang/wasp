module Wasp.Cli.Command.Db.Reset
  ( reset,
    ResetArgs (..),
    resetArgsParser,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Common (runTaskOrAbort)
import Wasp.Cli.Command.Db (makeDbCommand)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Util.Parser (ArgsParser (..), requireInteractiveTerminal)
import Wasp.Generator.DbGenerator.Common (ResetArgs (..))
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady))
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
import qualified Wasp.Util.Interactive as Interactive

reset :: ResetArgs -> Command ()
reset resetArgs = makeDbCommand [PrismaCliReady] $ \_appSpec projectRunConfig -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  unless (force resetArgs) $ do
    terminal <- requireInteractiveTerminal resetArgsParser
    confirmed <- liftIO $ Interactive.askForConfirmation terminal "This drops all the data in the database. Are you sure you want to continue?"
    unless confirmed $ do
      cliSendMessageC $ Msg.Info "Aborted."
      throwError CommandAborted

  runTaskOrAbort "Resetting the database" "Reset the database" "Failed to reset the database" $
    dbReset (ServerGenerator.makeEnvVars projectRunConfig) genProjectDir resetArgs {force = True}

resetArgsParser :: ArgsParser ResetArgs
resetArgsParser =
  ArgsParser "wasp db reset" $
    ResetArgs
      <$> Opt.switch
        ( Opt.long "force"
            <> Opt.help "Skip the confirmation prompt"
        )
