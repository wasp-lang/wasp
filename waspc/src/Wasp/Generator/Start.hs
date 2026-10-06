module Wasp.Generator.Start
  ( start,
  )
where

import Control.Concurrent (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (race)
import Control.Concurrent.Extra (threadDelay)
import Control.Monad (forever, void)
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig)
import Wasp.Generator.ServerGenerator.Start (ServerProcessController, startServer)
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import Wasp.Generator.WebAppGenerator.Start (startWebApp)
import qualified Wasp.Job as J
import qualified Wasp.Job.Output as Output
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util (secondsToMicroSeconds)

-- | This is a blocking action, that will start the processes that run web app and server.
--   It will run as long as one of those processes does not fail.
--   It alo receives 'onJobsQuietDown' IO action, which it executes every time all the processes
--   go quiet (don't produce any stdout/err) for some time (5s), after they have previously
--   produced some output.
start :: (WebAppRunConfig, ServerRunConfig) -> Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir GeneratedAppDir) -> ServerProcessController -> IO () -> IO (Either String ())
start (webAppRunConfig, serverRunConfig) waspProjectDir outDir serverProcessController onJobsQuietDown = do
  serverOrWebExitCode <-
    Output.withPrefixed $ \prefixed ->
      withJobsQuietDownListener onJobsQuietDown $ \notifyJobOutput -> do
        let sink jobKind stream output = notifyJobOutput >> prefixed jobKind stream output
        J.runJob (sink Output.Server) (startServer serverRunConfig outDir serverProcessController)
          `race` J.runJob (sink Output.WebApp) (startWebApp webAppRunConfig waspProjectDir)

  case serverOrWebExitCode of
    Left serverExitCode -> return $ Left $ "Server failed with exit code " ++ show serverExitCode ++ "."
    Right webAppExitCode -> return $ Left $ "Web app failed with exit code " ++ show webAppExitCode ++ "."

-- | Gives the action a function to call on every job output. Stops listening
-- once the action returns.
withJobsQuietDownListener :: IO () -> (IO () -> IO a) -> IO a
withJobsQuietDownListener onJobsQuietDown action = do
  jobOutputSignal <- newEmptyMVar
  either id absurd
    <$> action (void $ tryPutMVar jobOutputSignal ())
      `race` listenForJobsQuietDown jobOutputSignal onJobsQuietDown

listenForJobsQuietDown :: MVar () -> IO () -> IO Void
listenForJobsQuietDown jobOutputSignal onJobsQuietDown = forever $ do
  waitForJobOutput
  waitForPeriodOfSilence
  onJobsQuietDown
  where
    waitForJobOutput = takeMVar jobOutputSignal
    waitForPeriodOfSilence = do
      jobOutputOrTimeout <- waitForJobOutput `race` threadDelay (secondsToMicroSeconds 5)
      case jobOutputOrTimeout of
        Left _ -> waitForPeriodOfSilence
        Right _ -> return ()
