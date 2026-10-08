module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import qualified Control.Concurrent.Async as Async
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.ServerGenerator.Start (ServerProcessController, startServer)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)
import qualified Wasp.Job as J
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (secondsToMicroSeconds)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run until the web app exits. The server is kept up to date by the given controller.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: (WebAppRunConfig, ServerRunConfig) -> Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> IO () -> IO (Either String ())
start (webAppRunConfig, serverRunConfig) waspProjectDir outDir serverProcessController onJobsQuietDown = do
  webAppExitCode <-
    either absurd id
      <$> J.run
        ( withJobsQuietDownListener onJobsQuietDown $
            J.race
              (J.prefixWith J.Server $ startServer serverRunConfig outDir serverProcessController)
              (J.prefixWith J.WebApp $ startWebApp webAppRunConfig waspProjectDir)
        )

  return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."

-- | Calls 'onJobsQuietDown' every time the job goes quiet: it emits some
-- output, and then nothing for 5 seconds. Stops listening once the job
-- finishes.
withJobsQuietDownListener :: IO () -> J.Job a -> J.Job a
withJobsQuietDownListener onJobsQuietDown job = do
  jobOutputSignal <- liftIO newEmptyMVar
  either id absurd
    <$> J.race
      (J.onOutput (void $ tryPutMVar jobOutputSignal ()) job)
      (liftIO $ listenForJobsQuietDown jobOutputSignal onJobsQuietDown)

listenForJobsQuietDown :: MVar () -> IO () -> IO Void
listenForJobsQuietDown jobOutputSignal onJobsQuietDown = forever $ do
  waitForJobOutput
  waitForPeriodOfSilence
  onJobsQuietDown
  where
    waitForJobOutput = takeMVar jobOutputSignal
    waitForPeriodOfSilence = do
      jobOutputOrTimeout <- waitForJobOutput `Async.race` threadDelay (secondsToMicroSeconds 5)
      case jobOutputOrTimeout of
        Left _ -> waitForPeriodOfSilence
        Right _ -> return ()
