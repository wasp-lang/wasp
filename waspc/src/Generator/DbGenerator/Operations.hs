module Generator.DbGenerator.Operations
    ( migrateSave
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

migrateSave :: Path Abs (Dir ProjectRootDir) -> String -> IO (Either String ())
migrateSave projectDir migrationName = do
    chan <- newChan
    (_, dbExitCode) <- concurrently (handleJobMessages chan)
                                    (DbJobs.migrateSave projectDir migrationName chan)
    case dbExitCode of
        ExitSuccess -> return (Right ())
        ExitFailure code -> return $ Left $ "Migrate save failed with exit code: " ++ show code
    where
      -- TODO(matija): extract & reuse this for other operations.
        handleJobMessages :: Chan JobMessage -> IO ()
        handleJobMessages chan = do
            jobMsg <- readChan chan
            case J._data jobMsg of
                J.JobOutput {} -> printJobMessage jobMsg >> handleJobMessages chan
                J.JobExit {} -> return ()

