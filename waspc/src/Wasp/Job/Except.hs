module Wasp.Job.Except
  ( JobExcept,
    concurrent,
    run,
    fromJob,
  )
where

import Control.Concurrent (Chan)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Functor ((<&>))
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Wasp.Job (Job, JobMessage)

type JobExcept = Chan JobMessage -> ExceptT String IO ()

fromJob :: (Int -> String) -> Job -> JobExcept
fromJob f job chan =
  ExceptT $
    job chan <&> \case
      ExitSuccess -> Right ()
      ExitFailure code -> Left $ f code

concurrent :: [JobExcept] -> JobExcept
concurrent jobs chan =
  ExceptT $
    sequence_ <$> mapConcurrently (runExceptT . ($ chan)) jobs

run :: Chan JobMessage -> JobExcept -> IO (Either String ())
run chan jobExcept = runExceptT $ jobExcept chan
