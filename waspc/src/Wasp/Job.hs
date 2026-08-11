module Wasp.Job
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

-- | 'App' is the development server, which runs the client and the server in a
-- single process. 'WebApp' and 'Server' are the separate processes of a built
-- app.
data JobType = App | WebApp | Server | Db | Wasp deriving (Show, Eq, Ord, Bounded, Enum)
