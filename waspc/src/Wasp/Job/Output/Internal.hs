module Wasp.Job.Output.Internal
  ( getEventContent,
    getEventOutHandle,
  )
where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr, stdout)
import qualified Wasp.Job as Job

getEventOutHandle :: Job.JobEvent -> Handle
getEventOutHandle event = case Job._eventData event of
  Job.JobOutput outputKind _ ->
    case outputKind of
      Job.Stdout -> stdout
      Job.Stderr -> stderr
  Job.JobExited _ -> stdout

getEventContent :: Job.JobEvent -> T.Text
getEventContent event = case Job._eventData event of
  Job.JobOutput _ output -> output
  Job.JobExited ExitSuccess -> "Job exited successfully."
  Job.JobExited (ExitFailure exitCode) -> T.pack $ "Job failed with exit code " <> show exitCode
