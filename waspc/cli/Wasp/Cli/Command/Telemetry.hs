module Wasp.Cli.Command.Telemetry
  ( considerSendingData,
    telemetry,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Maybe (isJust)
import qualified StrongPath as SP
import qualified System.Environment as ENV
import Wasp.Cli.Command (Command, CommandError (..))
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
      maybeProjectCache <- liftIO $ TlmProject.readProjectTelemetryFile telemetryCacheDirPath projectHash
      for_ maybeProjectCache $ \projectCache -> do
        let maybeTimeOfLastSending = TlmProject.getTimeOfLastTelemetryDataSent projectCache
        for_ maybeTimeOfLastSending $ \timeOfLastSending -> do
          cliSendMessageC $ Msg.Info $ "Last time telemetry data was sent for this project: " ++ show timeOfLastSending

  cliSendMessageC $ Msg.Info "Our telemetry is anonymized and very limited in its scope: check https://wasp-lang.dev/docs/telemetry for more details."

-- | Sends telemetry data about the current Wasp project, if conditions are met.
-- If we are not in the Wasp project at the moment, nothing happens.
-- If telemetry data was already sent for this project in the last 12 hours, nothing happens.
-- If env var WASP_TELEMETRY_DISABLE is set, nothing happens.
considerSendingData :: Command.Call.Call -> Command ()
considerSendingData cmdCall = (`catchError` const (return ())) $ do
  telemetryDisabled <- liftIO isTelemetryDisabled
  when telemetryDisabled $ throwError $ CommandError "Telemetry failed" "Telemetry disabled by user."

  telemetryCacheDirPath <- liftIO ensureTelemetryCacheDirExists

  userSignature <- liftIO $ TlmUser.readOrCreateUserSignatureFile telemetryCacheDirPath

  maybeProjectHash <- (Just <$> TlmProject.getWaspProjectPathHash) `catchError` const (return Nothing)
  for_ maybeProjectHash $ \projectHash -> do
    liftIO $ TlmProject.considerSendingData telemetryCacheDirPath userSignature projectHash cmdCall
