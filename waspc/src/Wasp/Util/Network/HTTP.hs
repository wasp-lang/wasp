module Wasp.Util.Network.HTTP
  ( httpJSONThatCertainlyThrowsOnHttpError,
    catchRetryableHttpException,
  )
where

import Control.Arrow ()
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch, throwIO)

-- | Although docs say that httpJSON will thrown exception for all status codes except 2xx,
--   I believe I saw situations where it wasn't thrown and instead it was part of response,
--   so I wrote this extra wrapper that ensures that behaviour here just in case.
--   If we can prove that is not the case, and that exception is always thrown, we can remove this.
httpJSONThatCertainlyThrowsOnHttpError :: (MonadIO m, FromJSON a) => HTTP.Request -> m (HTTP.Response a)
httpJSONThatCertainlyThrowsOnHttpError request = do
  response <- HTTP.httpJSON request
  let statusCode = HTTP.getResponseStatusCode response
  if statusCode < 200 || statusCode >= 300
    then throwIO $ HTTP.HttpExceptionRequest request $ HTTP.C.StatusCodeException (void response) ""
    else pure response

catchRetryableHttpException :: MonadUnliftIO m => m a -> (HTTP.HttpException -> m a) -> m a
catchRetryableHttpException action handle =
  action
    `catch` (\e@(HTTP.HttpExceptionRequest _req HTTP.C.ResponseTimeout) -> handle e)
    `catch` (\e@(HTTP.HttpExceptionRequest _req HTTP.C.ConnectionTimeout) -> handle e)
    `catch` ( \e@(HTTP.HttpExceptionRequest _req (HTTP.C.StatusCodeException response _)) ->
                if shouldRetry response then handle e else throwIO e
            )
  where
    shouldRetry response =
      HTTP.getResponseStatusCode response `elem` retrayableHttpErrorStatusCodes
    retrayableHttpErrorStatusCodes = [503, 429, 408, 502, 504]
