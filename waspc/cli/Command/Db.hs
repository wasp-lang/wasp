module Command.Db
    ( runDbCommand
    ) where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent (Chan, newChan, readChan)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import System.Exit (ExitCode (..))

import StrongPath ((</>))
import Generator.ServerGenerator.Setup (setupServer)
import Generator.Job.IO (printJobMessage)
import qualified Generator.Job as J
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

    waspSaysC "Setting up database..."
    chan <- liftIO newChan
    -- NOTE(matija): What we do here is make sure that Prisma CLI is installed because db commands
    -- (e.g. migrate) depend on it. We run setupServer which does even more than that, so we could make
    -- this function more lightweight if needed.
    (_, dbSetupResult) <- liftIO (concurrently (handleJobMessages chan) (setupServer genProjectDir chan))
    case dbSetupResult of
        ExitSuccess -> waspSaysC "Database successfully set up!" >> cmd
        exitCode -> throwError $ CommandError $ dbSetupFailedMessage exitCode

    where
        dbSetupFailedMessage exitCode = "Database setup failed" ++
            case exitCode of
                ExitFailure code -> ": " ++ show code
                _ -> ""

        handleJobMessages :: Chan J.JobMessage -> IO ()
        handleJobMessages chan = do
            jobMsg <- readChan chan
            case J._data jobMsg of
                J.JobOutput {} -> printJobMessage jobMsg >> handleJobMessages chan
                J.JobExit {} -> return ()
