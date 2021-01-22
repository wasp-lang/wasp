module Command.Telemetry
    ( considerSendingData
    ) where

import           Control.Monad             (when)
import           Control.Monad.Except      (catchError, throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe                (isJust)
import qualified System.Environment        as ENV

import           Command                   (Command, CommandError (..))
import           Command.Telemetry.Common  (ensureTelemetryCacheDirExists)
import qualified Command.Telemetry.Project as TlmProject
import qualified Command.Telemetry.User    as TlmUser

-- | Sends telemetry data about the current Wasp project, if conditions are met.
-- If we are not in the Wasp project at the moment, nothing happens.
-- If telemetry data was already sent for this project in the last 12 hours, nothing happens.
-- If env var WASP_TELEMETRY_DISABLE is set, nothing happens.
considerSendingData :: Command ()
considerSendingData = (`catchError` const (return ())) $ do
    isTelemetryDisabled <- liftIO $ isJust <$> ENV.lookupEnv "WASP_TELEMETRY_DISABLE"
    when isTelemetryDisabled $ throwError $ CommandError "Telemetry disabled by user."

    telemetryCacheDirPath <- liftIO ensureTelemetryCacheDirExists

    userSignature <- liftIO $ TlmUser.readOrCreateUserSignatureFile telemetryCacheDirPath

    maybeProjectHash <- (Just <$> TlmProject.getWaspProjectPathHash) `catchError` const (return Nothing)
    case maybeProjectHash of
        Nothing -> return ()
        Just projectHash -> liftIO $ TlmProject.considerSendingData telemetryCacheDirPath userSignature projectHash
