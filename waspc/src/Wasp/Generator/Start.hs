module Wasp.Generator.Start
  ( start,
    StartOptions (..),
    defaultStartOptions,
  )
where

import Control.Concurrent (Chan, dupChan, newChan, readChan)
import Control.Concurrent.Async (concurrently, race, withAsync)
import Control.Concurrent.Extra (threadDelay)
import Control.Monad (void)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (StdinMode (..))
import Wasp.Project.Common (WaspProjectDir)

-- | How the started processes should behave towards the terminal. The
-- defaults are for a plain CLI run; the TUI overrides both, since it owns the
-- terminal and wants the output for itself.
data StartOptions = StartOptions
  { -- | What the started processes get as their standard input.
    stdinMode :: StdinMode,
    -- | Consumes the messages the started processes write, until one of them
    -- exits.
    consumeJobMessages :: Chan J.JobMessage -> IO ()
  }

defaultStartOptions :: StartOptions
defaultStartOptions =
  StartOptions
    { stdinMode = InheritStdin,
      consumeJobMessages = readJobMessagesAndPrintThemPrefixed
    }

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: StartOptions -> (WebAppRunConfig, ServerRunConfig) -> Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir GeneratedAppDir) -> IO () -> IO (Either String ())
start options (webAppRunConfig, serverRunConfig) waspProjectDir outDir onJobsQuietDown = do
  chan <- newChan

  let runStartJobs =
        startServer options.stdinMode serverRunConfig outDir chan
          `race` startWebApp options.stdinMode webAppRunConfig waspProjectDir chan

  quietDownChan <- dupChan chan
  -- Listening for jobs quieting down never ends, so we stop it once the jobs
  -- are done, instead of waiting for it.
  (serverOrWebExitCode, _) <-
    withAsync (listenForJobsQuietDown quietDownChan onJobsQuietDown) $ \_ ->
      runStartJobs `concurrently` options.consumeJobMessages chan

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
