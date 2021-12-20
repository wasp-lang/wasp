module Wasp.Cli.Command.Db
  ( runDbCommand,
    studio,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import System.Exit (ExitCode (..))
import Wasp.Cli.Command (Command, CommandError (..), runCommand)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import Wasp.Cli.Command.Compile (compile)
import qualified Wasp.Cli.Common as Common
import Wasp.Cli.Terminal (asWaspFailureMessage, asWaspStartMessage, asWaspSuccessMessage)
import Wasp.Generator.DbGenerator.Jobs (runStudio)
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.ServerGenerator.Setup (setupServer)

runDbCommand :: Command a -> IO ()
runDbCommand = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let genProjectDir =
        waspRoot </> Common.dotWaspDirInWaspProjectDir
          </> Common.generatedCodeDirInDotWaspDir

  -- NOTE(matija): First we need make sure the code is generated.
  compile

  waspSaysC $ asWaspStartMessage "Setting up database..."
  chan <- liftIO newChan
  -- NOTE(matija): What we do here is make sure that Prisma CLI is installed because db commands
  -- (e.g. migrate) depend on it. We run setupServer which does even more than that, so we could make
  -- this function more lightweight if needed.
  (_, dbSetupResult) <- liftIO (concurrently (readJobMessagesAndPrintThemPrefixed chan) (setupServer genProjectDir chan))
  case dbSetupResult of
    ExitSuccess -> waspSaysC (asWaspSuccessMessage "Database successfully set up!") >> cmd
    exitCode -> throwError $ CommandError $ asWaspFailureMessage $ dbSetupFailedMessage exitCode
  where
    dbSetupFailedMessage exitCode =
      "Database setup failed"
        ++ case exitCode of
          ExitFailure code -> ": " ++ show code
          _ -> ""

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
