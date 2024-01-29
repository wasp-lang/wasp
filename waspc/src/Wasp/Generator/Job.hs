module Wasp.Generator.Job
  ( Job,
    JobMessage (..),
    JobMessageData (..),
    JobOutputType (..),
    JobType (..),
  )
where

import Control.Concurrent (Chan)
import Data.Text (Text)
import System.Exit (ExitCode)

-- | Job is an IO action that communicates progress by writing messages to given channel
--   until it is done, when it returns exit code.
type Job = Chan JobMessage -> IO ExitCode

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

data JobType = WebApp | Server | Db deriving (Show, Eq, Ord, Bounded, Enum)
