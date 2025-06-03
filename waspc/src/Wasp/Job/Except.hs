module Wasp.Job.Except
  ( JobExcept,
    concurrent,
    run,
    fromJob,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently, mapConcurrently)
import Control.Monad.Except (ExceptT (ExceptT), MonadIO (liftIO), runExceptT)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Wasp.Job (Job)
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)

type JobExcept = ExceptT String IO ()

fromJob :: (Int -> String) -> Job -> JobExcept
fromJob f job = ExceptT $ do
  chan <- liftIO newChan
  (_, result) <-
    concurrently
      (readJobMessagesAndPrintThemPrefixed chan)
      (job chan)

  return $ case result of
    ExitSuccess -> Right ()
    ExitFailure code -> Left $ f code

concurrent :: [JobExcept] -> JobExcept
concurrent jobs =
  ExceptT $ sequence_ <$> mapConcurrently runExceptT jobs

run :: JobExcept -> IO (Either String ())
run jobExcept = runExceptT jobExcept
