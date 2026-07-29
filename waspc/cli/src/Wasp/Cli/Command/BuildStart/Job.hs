module Wasp.Cli.Command.BuildStart.Job
  ( JobExecution,
    run,
    race,
  )
where

import Control.Concurrent (Chan)
import qualified Control.Concurrent.Async as Async
import Data.Functor ((<&>))
import System.Exit (ExitCode (..))
import qualified Wasp.Job as Job

type JobExecution = Chan Job.JobEvent -> IO (Either String ())

run :: (Int -> String) -> Job.Job -> JobExecution
run exitCodeToErrorMessage job events =
  Job.runJob job events
    <&> fromExitCode exitCodeToErrorMessage
  where
    fromExitCode _ ExitSuccess = Right ()
    fromExitCode toErrorMessage (ExitFailure code) = Left $ toErrorMessage code

race :: JobExecution -> JobExecution -> JobExecution
race first second events =
  either id id
    <$> Async.race
      (first events)
      (second events)
