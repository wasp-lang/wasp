module Wasp.Job.Output.Event (JobEvent (..), JobEventData (..), JobOutputKind (..), getEventContent, getEventOutHandle) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr, stdout)
import Wasp.Job.Kind (JobKind)

data JobEvent = JobEvent
  { _eventData :: JobEventData,
    _jobKind :: JobKind
  }
  deriving (Show)

data JobEventData
  = JobOutput JobOutputKind Text
  | JobExited ExitCode
  deriving (Show)

data JobOutputKind = Stdout | Stderr deriving (Show, Eq)

getEventOutHandle :: JobEvent -> Handle
getEventOutHandle event = case _eventData event of
  JobOutput outputKind _ ->
    case outputKind of
      Stdout -> stdout
      Stderr -> stderr
  JobExited _ -> stdout

getEventContent :: JobEvent -> T.Text
getEventContent event = case _eventData event of
  JobOutput _ output -> output
  JobExited ExitSuccess -> "Job exited successfully."
  JobExited (ExitFailure exitCode) -> T.pack $ "Job failed with exit code " <> show exitCode
