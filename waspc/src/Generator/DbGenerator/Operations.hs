module Generator.DbGenerator.Operations
    ( migrateSave
    , migrateUp
    , generateClient
    ) where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import System.Exit (ExitCode (..))

import StrongPath (Abs, Dir, Path)
import Generator.Common (ProjectRootDir)
import Generator.Job.IO (printJobMessage)
import qualified Generator.Job as J
import Generator.Job (JobMessage)
import qualified Generator.DbGenerator.Jobs as DbJobs

printJobMsgsUntilExitReceived :: Chan JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
    jobMsg <- readChan chan
    case J._data jobMsg of
        J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
        J.JobExit {} -> return ()

-- | Checks for the changes in db schema file and creates and saves db migration info, but it
-- does not execute it.
migrateSave :: Path Abs (Dir ProjectRootDir) -> String -> IO (Either String ())
migrateSave projectDir migrationName = do
    chan <- newChan
    (_, dbExitCode) <- concurrently (printJobMsgsUntilExitReceived chan)
                                    (DbJobs.migrateSave projectDir migrationName chan)
    case dbExitCode of
        ExitSuccess -> return (Right ())
        ExitFailure code -> return $ Left $ "Migrate save failed with exit code: " ++ show code

migrateUp :: Path Abs (Dir ProjectRootDir) -> IO (Either String ())
migrateUp projectDir = do
    chan <- newChan
    (_, dbExitCode) <- concurrently (printJobMsgsUntilExitReceived chan)
                                    (DbJobs.migrateUp projectDir chan)
    case dbExitCode of
        ExitSuccess -> return (Right ())
        ExitFailure code -> return $ Left $ "Migrate up failed with exit code: " ++ show code

generateClient :: Path Abs (Dir ProjectRootDir) -> IO (Either String ())
generateClient projectDir = do
    chan <- newChan
    (_, dbExitCode) <- concurrently (printJobMsgsUntilExitReceived chan)
                                    (DbJobs.generateClient projectDir chan)
    case dbExitCode of
        ExitSuccess -> return (Right ())
        ExitFailure code -> return $ Left $ "Client generation failed with exit code: " ++ show code

