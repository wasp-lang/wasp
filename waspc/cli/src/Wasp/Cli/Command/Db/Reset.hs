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
import Wasp.Cli.Command (Command, commandError, getConsole, require)
import Wasp.Cli.Command.Db (makeDbCommand)
import Wasp.Cli.Command.Message (runTaskC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Console (Console (..))
import Wasp.Cli.Util.Parser (ArgsParser (..))
import Wasp.Generator.DbGenerator.Common (ResetArgs (..))
import Wasp.Generator.DbGenerator.Operations (dbReset)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady))
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)

reset :: ResetArgs -> Command ()
reset resetArgs = makeDbCommand [PrismaCliReady] $ \_appSpec projectRunConfig -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir =
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  console <- getConsole
  runTaskC "Resetting the database" "Reset the database" $
    liftIO (dbReset console.writeRawOutput (ServerGenerator.makeEnvVars projectRunConfig) genProjectDir resetArgs)
      >>= either (throwError . commandError "Failed to reset the database") return

resetArgsParser :: ArgsParser ResetArgs
resetArgsParser =
  ArgsParser "wasp db reset" $
    ResetArgs
      <$> Opt.switch
        ( Opt.long "force"
            <> Opt.help "Skip the confirmation prompt"
        )
