module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (Chan, dupChan, newChan, readChan)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.Extra (threadDelay)
import Control.Monad (void)
import StrongPath (Abs, Dir, Path')
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir)

-- | This is a blocking action that runs the generated app in development mode.
--   It will run as long as the process running the app does not fail.
--   A single Vite process runs both the client and the server: the client
--   through Vite's dev server, the server through Vite's `server` environment.
--   It also receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: Path' Abs (Dir WaspProjectDir) -> IO () -> IO (Either String ())
start waspProjectDir onJobsQuietDown = do
  chan <- newChan
  ((appExitCode, _), _) <-
    startApp waspProjectDir chan
      `concurrently` readJobMessagesAndPrintThemPrefixed chan
      `concurrently` (dupChan chan >>= (`listenForJobsQuietDown` onJobsQuietDown))
  return $ Left $ "App failed with exit code " ++ show appExitCode ++ "."

startApp :: Path' Abs (Dir WaspProjectDir) -> J.Job
startApp waspProjectDir = runNodeCommandAsJob waspProjectDir "npx" ["vite"] J.App

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
