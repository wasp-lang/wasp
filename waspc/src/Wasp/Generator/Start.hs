module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (race)
import Control.Concurrent.Extra (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (fuseUpstream)
import qualified Data.Conduit.List as CL
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.ServerGenerator.Start (startServer)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)
import qualified Wasp.Job.Kind as Kind
import qualified Wasp.Job.Output as Output
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (secondsToMicroSeconds)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: (WebAppRunConfig, ServerRunConfig) -> Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir GeneratedAppDir) -> IO () -> IO (Either String ())
start (webAppRunConfig, serverRunConfig) waspProjectDir outDir onJobsQuietDown = do
  jobActivity <- newChan
  let reportingActivity job = job `fuseUpstream` CL.iterM (const $ liftIO $ writeChan jobActivity ())

  serverOrWebExitCode <-
    either absurd id
      <$> race
        (listenForJobsQuietDown jobActivity onJobsQuietDown)
        ( Output.raceAndPrintPrefixedOutput
            (Kind.Server, reportingActivity $ startServer serverRunConfig outDir)
            (Kind.WebApp, reportingActivity $ startWebApp webAppRunConfig waspProjectDir)
        )

  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."

listenForJobsQuietDown :: Chan () -> IO () -> IO Void
listenForJobsQuietDown jobActivity onJobsQuietDown = forever $ do
  waitForJobActivity
  waitForPeriodOfSilence
  onJobsQuietDown
  where
    waitForJobActivity = readChan jobActivity
    waitForPeriodOfSilence = do
      activityOrTimeout <- readChan jobActivity `race` threadDelay (secondsToMicroSeconds 5)
      case activityOrTimeout of
        Left _ -> waitForPeriodOfSilence
        Right _ -> return ()
