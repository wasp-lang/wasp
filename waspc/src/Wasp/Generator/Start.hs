module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (Chan, dupChan, newChan, readChan)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.Extra (threadDelay)
import Control.Monad (void)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.WebAppGenerator.Start (startWebApp)
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Project.Common (WaspProjectDir)

-- | This is a blocking action, that will start the process running the app (one
--   process serves the whole app: its pages, its API, and its jobs).
--   It will run as long as that process does not fail.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: Path' Abs (Dir WaspProjectDir) -> IO () -> IO (Either String ())
start waspProjectDir onJobsQuietDown = do
  chan <- newChan
  ((appExitCode, _), _) <-
    startWebApp waspProjectDir chan
      `concurrently` readJobMessagesAndPrintThemPrefixed chan
      `concurrently` (dupChan chan >>= (`listenForJobsQuietDown` onJobsQuietDown))
  return $ Left $ "App failed with exit code " ++ show appExitCode ++ "."

listenForJobsQuietDown :: Chan J.JobMessage -> IO () -> IO ()
listenForJobsQuietDown jobsChan onJobsQuietDown = do
  waitForJobMsg
  waitForPeriodOfSilence
  onJobsQuietDown
  listenForJobsQuietDown jobsChan onJobsQuietDown
  where
    waitForJobMsg = void $ readChan jobsChan
    waitForPeriodOfSilence = do
      jobMsgOrTimeout <- readChan jobsChan `race` threadDelay (secondsAsMs 5)
      case jobMsgOrTimeout of
        Left _ -> waitForPeriodOfSilence
        Right _ -> return ()
    secondsAsMs s = s * 1000000
