module Wasp.Job
  ( Job,
    JobOutputSink,
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

newtype JobOutputSink = JobOutputSink (JobOutputType -> Text -> IO ())

-- Build Jobs with makeJob: readers wait for JobExit and can hang without it.
-- Inner actions receive only an output sink, so makeJob remains the sole owner
-- of JobExit emission.
makeJob :: JobType -> (JobOutputSink -> IO ExitCode) -> Job
makeJob jobType run chan =
  run (JobOutputSink $ writeOutput jobType chan) >>= emitJobExit jobType chan

emitJobExit :: JobType -> Chan JobMessage -> ExitCode -> IO ExitCode
emitJobExit jobType chan exitCode = do
  writeChan chan $
    JobMessage
      { _data = JobExit exitCode,
        _jobType = jobType
      }
  return exitCode

writeJobOutput :: JobOutputSink -> JobOutputType -> Text -> IO ()
writeJobOutput (JobOutputSink writeOutputToSink) = writeOutputToSink

writeOutput :: JobType -> Chan JobMessage -> JobOutputType -> Text -> IO ()
writeOutput jobType chan outputType output =
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
