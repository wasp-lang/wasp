module Command.Db
    ( runDbCommand
    , studio
    ) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent (newChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import System.Exit (ExitCode (..))

import StrongPath ((</>))
import Generator.ServerGenerator.Setup (setupServer)
import Generator.DbGenerator.Jobs (runStudio)
import Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Command (Command, CommandError(..), runCommand)
import Command.Compile (compile)
import Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import qualified Common

runDbCommand :: Command a -> IO ()
runDbCommand = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
    waspRoot <- findWaspProjectRootDirFromCwd
    let genProjectDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </>
                        Common.generatedCodeDirInDotWaspDir

    -- NOTE(matija): First we need make sure the code is generated.
    compile

    waspSaysC "\nSetting up database..."
    chan <- liftIO newChan
    -- NOTE(matija): What we do here is make sure that Prisma CLI is installed because db commands
    -- (e.g. migrate) depend on it. We run setupServer which does even more than that, so we could make
    -- this function more lightweight if needed.
    (_, dbSetupResult) <- liftIO (concurrently (readJobMessagesAndPrintThemPrefixed chan) (setupServer genProjectDir chan))
    case dbSetupResult of
        ExitSuccess -> waspSaysC "\nDatabase successfully set up!" >> cmd
        exitCode -> throwError $ CommandError $ dbSetupFailedMessage exitCode

    where
        dbSetupFailedMessage exitCode = "\nDatabase setup failed" ++
            case exitCode of
                ExitFailure code -> ": " ++ show code
                _ -> ""

-- TODO(matija): should we extract this into a separate file, like we did for migrate?
studio :: Command ()
studio = do
    waspProjectDir <- findWaspProjectRootDirFromCwd
    let genProjectDir = waspProjectDir </> Common.dotWaspDirInWaspProjectDir
                        </> Common.generatedCodeDirInDotWaspDir

    waspSaysC "Running studio..."
    chan <- liftIO newChan

    _ <- liftIO $ concurrently (readJobMessagesAndPrintThemPrefixed chan) (runStudio genProjectDir chan)
    error "This should never happen, studio should never stop."
