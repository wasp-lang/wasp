module Wasp.Cli.Command.BuildStart.Job
  ( BuildStartJob,
    JobExecution,
    make,
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

data BuildStartJob = BuildStartJob Job.Job (Int -> String)

type JobExecution = Chan Job.JobEvent -> ExceptT String IO ()

make :: (Int -> String) -> Job.Job -> BuildStartJob
make = flip BuildStartJob

run :: BuildStartJob -> JobExecution
run (BuildStartJob job exitCodeToErrorMessage) events =
  ExceptT $
    Job.runJob job events
      <&> fromExitCode exitCodeToErrorMessage
  where
    fromExitCode _ ExitSuccess = Right ()
    fromExitCode toErrorMessage (ExitFailure code) = Left $ toErrorMessage code

race :: BuildStartJob -> BuildStartJob -> JobExecution
race first second events =
  ExceptT $
    either id id
      <$> Async.race
        (runExceptT $ run first events)
        (runExceptT $ run second events)
