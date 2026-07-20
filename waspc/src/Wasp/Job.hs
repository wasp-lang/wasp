module Wasp.Job
  ( Job,
    JobOutputStreamer,
    makeJob,
    writeJobOutput,
    JobMessage (..),
    JobMessageData (..),
    JobOutputType (..),
    JobType (..),
  )
where

import Control.Concurrent (Chan, writeChan)
import Data.Text (Text)
import System.Exit (ExitCode)

-- | Job is an IO action that communicates progress by writing messages to given channel
--   until it is done, when it returns exit code.
type Job = Chan JobMessage -> IO ExitCode

-- Build Jobs with makeJob: readers wait for JobExit and can hang without it.
-- JobOutputStreamer is for inner processes: they share output, but must not end the Job.
-- Streamers can still emit JobExit; we don't enforce this to keep the implementation simple.
type JobOutputStreamer = Chan JobMessage -> IO ExitCode

makeJob :: JobType -> JobOutputStreamer -> Job
makeJob jobType streamer chan =
  streamer chan >>= emitJobExit jobType chan

emitJobExit :: JobType -> Chan JobMessage -> ExitCode -> IO ExitCode
emitJobExit jobType chan exitCode = do
  writeChan chan $
    JobMessage
      { _data = JobExit exitCode,
        _jobType = jobType
      }
  return exitCode

writeJobOutput :: JobType -> JobOutputType -> Text -> Chan JobMessage -> IO ()
writeJobOutput jobType outputType output chan =
  writeChan chan $
    JobMessage
      { _data = JobOutput output outputType,
        _jobType = jobType
      }

data JobMessage = JobMessage
  { _data :: JobMessageData,
    _jobType :: JobType
  }
  deriving (Show)

data JobMessageData
  = JobOutput Text JobOutputType
  | JobExit ExitCode
  deriving (Show)

data JobOutputType = Stdout | Stderr deriving (Show, Eq)

data JobType = WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)
