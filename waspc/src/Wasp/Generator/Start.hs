module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (Chan, dupChan, newChan, readChan)
import Control.Concurrent.Async (concurrently, race)
import Control.Concurrent.Extra (threadDelay)
import Control.Monad (void)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: Path' Abs (Dir ProjectRootDir) -> IO () -> IO (Either String ())
start projectDir onJobsQuietDown = do
  chan <- newChan
  let runStartJobs = startServer projectDir chan `race` startWebApp projectDir chan
  ((serverOrWebExitCode, _), _) <-
    runStartJobs
      `concurrently` readJobMessagesAndPrintThemPrefixed chan
      `concurrently` (dupChan chan >>= (`listenForJobsQuietDown` onJobsQuietDown))
  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."

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
