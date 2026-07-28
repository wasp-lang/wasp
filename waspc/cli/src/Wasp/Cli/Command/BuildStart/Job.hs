module Wasp.Cli.Command.BuildStart.Job
  ( JobExecution,
    run,
    race,
  )
where

import Control.Concurrent (Chan)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Functor ((<&>))
import System.Exit (ExitCode (..))
import qualified Wasp.Job as Job

type JobExecution = Chan Job.JobEvent -> ExceptT String IO ()

run :: (Int -> String) -> Job.Job -> JobExecution
run exitCodeToErrorMessage job events =
  ExceptT $
    Job.runJob job events
      <&> fromExitCode exitCodeToErrorMessage
  where
    fromExitCode _ ExitSuccess = Right ()
    fromExitCode toErrorMessage (ExitFailure code) = Left $ toErrorMessage code

race :: JobExecution -> JobExecution -> JobExecution
race first second events =
  ExceptT $
    either id id
      <$> Async.race
        (runExceptT $ first events)
        (runExceptT $ second events)
