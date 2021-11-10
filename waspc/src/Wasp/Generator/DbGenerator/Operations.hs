module Generator.DbGenerator.Operations
  ( migrateDev,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Generator.Common (ProjectRootDir)
import qualified Generator.DbGenerator.Jobs as DbJobs
import Generator.Job (JobMessage)
import qualified Generator.Job as J
import Generator.Job.IO (printJobMessage)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))

printJobMsgsUntilExitReceived :: Chan JobMessage -> IO ()
printJobMsgsUntilExitReceived chan = do
  jobMsg <- readChan chan
  case J._data jobMsg of
    J.JobOutput {} -> printJobMessage jobMsg >> printJobMsgsUntilExitReceived chan
    J.JobExit {} -> return ()

migrateDev :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
migrateDev projectDir = do
  chan <- newChan
  (_, dbExitCode) <-
    concurrently
      (printJobMsgsUntilExitReceived chan)
      (DbJobs.migrateDev projectDir chan)
  case dbExitCode of
    ExitSuccess -> return (Right ())
    ExitFailure code -> return $ Left $ "Migrate (dev) failed with exit code: " ++ show code
