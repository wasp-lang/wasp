module Generator.Job.Process
    ( runProcessAsJob
    , runNodeCommandAsJob
    ) where

import           Control.Concurrent    (writeChan)
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit          (runConduit, (.|))
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Process  as CP
import qualified System.Process        as P

import qualified Generator.Job         as J
import           StrongPath            (Abs, Dir, Path)
import qualified StrongPath            as SP


-- | Runs a given process while streaming its stderr and stdout to provided channel.
--   Returns exit code of the process once it finishes, and also sends it to he channel.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType = \chan -> do
    (CP.ClosedStream, stdoutStream, stderrStream, processHandle) <- CP.streamingProcess process

    -- TODO: Do I need to use Concurrently to run concurrently these three below:
    --   stdout, sdterr, and waiting for process? They do it in documentation/tutorial:
    --   https://github.com/snoyberg/conduit/blob/master/PROCESS.md .
    --   But for me it works fine without it, for now.

    runConduit $ stdoutStream .| CL.mapM_
        (\bs -> writeChan chan $ J.JobMessage { J._data = J.JobOutput (BS.unpack bs) J.Stdout
                                              , J._jobType = jobType })

    runConduit $ stderrStream .| CL.mapM_
        (\bs -> writeChan chan $ J.JobMessage { J._data = J.JobOutput (BS.unpack bs) J.Stderr
                                              , J._jobType = jobType })

    exitCode <- CP.waitForStreamingProcess processHandle

    writeChan chan $ J.JobMessage { J._data = J.JobExit exitCode
                                  , J._jobType = jobType }

    return exitCode

runNodeCommandAsJob :: Path Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob fromDir command args jobType = do
    -- TODO: Check npm/node version.
    let process = (P.proc command args) { P.cwd = Just $ SP.toFilePath fromDir }
    runProcessAsJob process jobType

