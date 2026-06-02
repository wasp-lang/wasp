module Wasp.Cli.Command.Telemetry
  ( considerSendingData,
    runWithTelemetry,
    telemetry,
    parserInfo,
  )
where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Monad (unless, void, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import qualified Options.Applicative as Opt
import qualified StrongPath as SP
import qualified System.Environment as ENV
import Wasp.Cli.Command (Command, CommandError (..), runCommand)
import qualified Wasp.Cli.Command.Call as Command.Call
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Telemetry.Common (ensureTelemetryCacheDirExists)
import qualified Wasp.Cli.Command.Telemetry.Project as TlmProject
import qualified Wasp.Cli.Command.Telemetry.User as TlmUser
import qualified Wasp.Message as Msg

isTelemetryDisabled :: IO Bool
isTelemetryDisabled = isJust <$> ENV.lookupEnv "WASP_TELEMETRY_DISABLE"

-- | Prints basic information about the stauts of telemetry.
telemetry :: Command ()
telemetry = do
  telemetryDisabled <- liftIO isTelemetryDisabled
  cliSendMessageC $
    Msg.Info $
      "Telemetry is currently: "
        <> ( if telemetryDisabled
               then "DISABLED"
               else "ENABLED"
           )

  unless telemetryDisabled $ do
    telemetryCacheDirPath <- liftIO ensureTelemetryCacheDirExists
    cliSendMessageC $ Msg.Info $ "Telemetry cache directory: " ++ SP.toFilePath telemetryCacheDirPath

    maybeProjectHash <- (Just <$> TlmProject.getWaspProjectPathHash) `catchError` const (return Nothing)
    for_ maybeProjectHash $ \projectHash -> do
      maybeProjectCache <- liftIO $ TlmProject.readProjectTelemetryCacheFile telemetryCacheDirPath projectHash
      for_ maybeProjectCache $ \projectCache -> do
        let maybeTimeOfLastSending = TlmProject.getTimeOfLastTelemetryDataSent projectCache
        for_ maybeTimeOfLastSending $ \timeOfLastSending -> do
          cliSendMessageC $ Msg.Info $ "Last time telemetry data was sent for this project: " ++ show timeOfLastSending

  cliSendMessageC $ Msg.Info "Our telemetry is anonymized and very limited in its scope: check https://wasp.sh/docs/telemetry for more details."

parserInfo :: Opt.ParserInfo (IO ())
parserInfo =
  Opt.info
    (pure $ runWithTelemetry Command.Call.Other (runCommand telemetry))
    (Opt.progDesc "Print telemetry status.")

-- | Runs an IO action while, concurrently, considering whether to send
-- telemetry data for the given command call. Telemetry is given up to one
-- extra second to finish after the action completes, and any telemetry errors
-- are silenced. Commands use this to report their own telemetry.
runWithTelemetry :: Command.Call.Call -> IO () -> IO ()
runWithTelemetry call action = do
  telemetryThread <- Async.async $ runCommand $ considerSendingData call
  action
  -- If sending of telemetry data is still not done 1 second since the command
  -- finished, abort it. We also catch and silence any errors it might throw.
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

-- | Sends telemetry data about the current Wasp project, if conditions are met.
-- If we are not in the Wasp project at the moment, nothing happens.
-- If telemetry data was already sent for this project in the last 12 hours, nothing happens.
-- If env var WASP_TELEMETRY_DISABLE is set, nothing happens.
considerSendingData :: Command.Call.Call -> Command ()
considerSendingData cmdCall = (`catchError` const (return ())) $ do
  telemetryDisabled <- liftIO isTelemetryDisabled
  when telemetryDisabled $ throwError $ CommandError "Telemetry failed" "Telemetry disabled by user."

  telemetryCacheDirPath <- liftIO ensureTelemetryCacheDirExists

  userSignature <- liftIO $ TlmUser.obtainUserSignature telemetryCacheDirPath

  maybeProjectHash <- (Just <$> TlmProject.getWaspProjectPathHash) `catchError` const (return Nothing)
  for_ maybeProjectHash $ \projectHash -> do
    liftIO $ TlmProject.considerSendingData telemetryCacheDirPath userSignature projectHash cmdCall
