module Wasp.Generator.DbGenerator.Operations
  ( migrateDev,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.DbGenerator.Jobs as DbJobs
import Wasp.Generator.Job (JobMessage)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (printJobMessage)

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
