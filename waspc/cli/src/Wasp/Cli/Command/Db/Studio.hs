module Wasp.Cli.Command.Db.Studio
  ( studio,
    studioArgsParser,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Db (makeDbCommand)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Util.Parser (ArgsParser (..))
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady))
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

studio :: Command ()
studio = makeDbCommand [PrismaCliReady] $ \_appSpec -> do
  InWaspProject waspProjectDir <- require
  ValidNodeAndNpm <- require
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Running studio..."

  chan <- liftIO newChan
  _ <- liftIO $ readJobMessagesAndPrintThemPrefixed chan `concurrently` runStudio genProjectDir chan

  error "This should never happen, studio should never stop."

studioArgsParser :: ArgsParser ()
studioArgsParser = ArgsParser "wasp db studio" (pure ())
