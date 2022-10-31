module Wasp.Generator.Job.Common
  ( getJobMessageOutHandle,
    getJobMessageContent,
  )
where

import qualified Data.Text as T
import System.Exit (ExitCode (..))
import System.IO (Handle, stderr, stdout)
import qualified Wasp.Generator.Job as J

getJobMessageOutHandle :: J.JobMessage -> Handle
getJobMessageOutHandle jobMsg = case J._data jobMsg of
  J.JobOutput _ outputType ->
    case outputType of
      J.Stdout -> stdout
      J.Stderr -> stderr
  J.JobExit _ -> stdout

getJobMessageContent :: J.JobMessage -> T.Text
getJobMessageContent jobMsg = case J._data jobMsg of
  J.JobOutput output _ -> output
  J.JobExit ExitSuccess -> "Job exited successfully."
  J.JobExit (ExitFailure exitCode) -> T.pack $ "Job failed with exit code " <> show exitCode
