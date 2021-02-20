module Command.Telemetry
    ( considerSendingData
    , telemetry
    ) where

import           Control.Monad             (when)
import           Control.Monad.Except      (catchError, throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe                (isJust)
import qualified System.Environment        as ENV

import           Command                   (Command, CommandError (..))
import           Command.Common            (waspSaysC)
import qualified Command.Call
import           Command.Telemetry.Common  (ensureTelemetryCacheDirExists)
import qualified Command.Telemetry.Project as TlmProject
import qualified Command.Telemetry.User    as TlmUser

isTelemetryDisabled :: IO Bool
isTelemetryDisabled = isJust <$> ENV.lookupEnv "WASP_TELEMETRY_DISABLE"

telemetry :: Command ()
telemetry = do
    telemetryDisabled <- liftIO isTelemetryDisabled
    waspSaysC $ "Telemetry is currently: " <> (if telemetryDisabled
        then "DISABLED"
        else "ENABLED") 

-- | Sends telemetry data about the current Wasp project, if conditions are met.
-- If we are not in the Wasp project at the moment, nothing happens.
-- If telemetry data was already sent for this project in the last 12 hours, nothing happens.
-- If env var WASP_TELEMETRY_DISABLE is set, nothing happens.
considerSendingData :: Command.Call.Call -> Command ()
considerSendingData cmdCall = (`catchError` const (return ())) $ do
    telemetryDisabled <- liftIO isTelemetryDisabled
    when telemetryDisabled $ throwError $ CommandError "Telemetry disabled by user."

    telemetryCacheDirPath <- liftIO ensureTelemetryCacheDirExists

    userSignature <- liftIO $ TlmUser.readOrCreateUserSignatureFile telemetryCacheDirPath

    maybeProjectHash <- (Just <$> TlmProject.getWaspProjectPathHash) `catchError` const (return Nothing)
    case maybeProjectHash of
        Nothing -> return ()
        Just projectHash -> liftIO $ TlmProject.considerSendingData telemetryCacheDirPath userSignature projectHash cmdCall
