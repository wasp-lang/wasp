module Wasp.Job.Except
  ( JobExcept,
    fromExitCode,
    toJobExcept,
    race_,
  )
where

import Control.Concurrent (Chan)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Functor (void, (<&>))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Wasp.Job (Job)
import qualified Wasp.Job as J

type JobExcept = Chan J.JobMessage -> ExceptT String IO ()

toJobExcept :: (Int -> String) -> Job -> JobExcept
toJobExcept exitCodeToErrorMessage action chan =
  ExceptT $
    action chan
      <&> fromExitCode exitCodeToErrorMessage

fromExitCode :: (Int -> String) -> ExitCode -> Either String ()
fromExitCode _ ExitSuccess = Right ()
fromExitCode exitCodeToErrorMessage (ExitFailure code) = Left $ exitCodeToErrorMessage code

race_ :: JobExcept -> JobExcept -> JobExcept
race_ except1 except2 chan =
  ExceptT $
    void . unwrapEither
      <$> Async.race
        (runExceptT $ except1 chan)
        (runExceptT $ except2 chan)
  where
    unwrapEither = either id id
