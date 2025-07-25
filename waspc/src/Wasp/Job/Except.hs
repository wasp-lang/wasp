module Wasp.Job.Except
  ( ExceptJob,
    toExceptJob,
    race_,
  )
where

import Control.Concurrent (Chan)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Functor ((<&>))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Wasp.Job (Job)
import qualified Wasp.Job as J

type ExceptJob = Chan J.JobMessage -> ExceptT String IO ()

toExceptJob :: (Int -> String) -> Job -> ExceptJob
toExceptJob exitCodeToErrorMessage job chan =
  ExceptT $
    job chan
      <&> fromExitCode exitCodeToErrorMessage
  where
    fromExitCode :: (Int -> String) -> ExitCode -> Either String ()
    fromExitCode _ ExitSuccess = Right ()
    fromExitCode toErrorMessage (ExitFailure code) = Left $ toErrorMessage code

race_ :: ExceptJob -> ExceptJob -> ExceptJob
race_ except1 except2 chan =
  ExceptT $
    unwrapEither
      <$> Async.race
        (runExceptT $ except1 chan)
        (runExceptT $ except2 chan)
  where
    unwrapEither = either id id
