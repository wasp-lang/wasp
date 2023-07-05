module Wasp.Util.Network.HTTP
  ( catchRetryableHttpException,
  )
where

import Control.Arrow ()
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch, throwIO)

catchRetryableHttpException :: MonadUnliftIO m => m a -> (HTTP.HttpException -> m a) -> m a
catchRetryableHttpException action handle =
  action
    `catch` ( \e -> case e of
                HTTP.HttpExceptionRequest _req HTTP.C.ResponseTimeout -> handle e
                HTTP.HttpExceptionRequest _req HTTP.C.ConnectionTimeout -> handle e
                HTTP.HttpExceptionRequest _req (HTTP.C.StatusCodeException response _)
                  | shouldRetry response -> handle e
                _nonRetrayableException -> throwIO e
            )
  where
    shouldRetry response =
      HTTP.getResponseStatusCode response `elem` retrayableHttpErrorStatusCodes

    retrayableHttpErrorStatusCodes = [503, 429, 408, 502, 504]
