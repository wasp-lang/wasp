module Wasp.Cli.Command.Db
  ( runDbCommand,
    studio,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, runCommand)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import Wasp.Cli.Command.Compile (compile)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspStartMessage)
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)

runDbCommand :: Command a -> IO ()
runDbCommand = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
  -- Ensure code is generated and npm dependencies are installed.
  compile
  cmd

-- TODO(matija): should we extract this into a separate file, like we did for migrate?
studio :: Command ()
studio = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  let genProjectDir =
        waspProjectDir </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  waspSaysC $ asWaspStartMessage "Running studio..."
  chan <- liftIO newChan

  _ <- liftIO $ concurrently (readJobMessagesAndPrintThemPrefixed chan) (runStudio genProjectDir chan)
  error "This should never happen, studio should never stop."
