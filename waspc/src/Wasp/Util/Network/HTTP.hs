module Wasp.Util.Network.HTTP
  ( catchRetryableHttpException,
    getHttpExceptionStatusCode,
    httpJSONThatThrowsIfNot2xx,
    checkUrlExists,
  )
where

import Control.Arrow ()
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Network.HTTP.Conduit as HTTP.C
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Status (statusIsSuccessful)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catch, throwIO)

catchRetryableHttpException :: (MonadUnliftIO m) => m a -> (HTTP.HttpException -> m a) -> m a
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

getHttpExceptionStatusCode :: HTTP.HttpException -> Maybe Int
getHttpExceptionStatusCode = \case
  HTTP.HttpExceptionRequest _req (HTTP.C.StatusCodeException response _) ->
    Just $ HTTP.getResponseStatusCode response
  _otherwise -> Nothing

-- | Throws an HttpException if status is not 2xx.
-- Returns JSON parse error as Left if JSON parsing failed.
httpJSONThatThrowsIfNot2xx :: (MonadIO m, FromJSON a) => HTTP.Request -> m (Either String a)
httpJSONThatThrowsIfNot2xx request = do
  response <- HTTP.httpLBS request

  let statusCode = HTTP.getResponseStatusCode response
  when (statusCode < 200 || statusCode >= 300) $
    throwIO $
      HTTP.HttpExceptionRequest request (HTTP.C.StatusCodeException (void response) "")

  return $ Aeson.eitherDecode $ HTTP.getResponseBody response

checkUrlExists :: (MonadIO m) => String -> m Bool
checkUrlExists url = liftIO $ do
  res <- httpHeadRequest url
  return $ statusIsSuccessful $ HTTP.getResponseStatus res

httpHeadRequest :: String -> IO (HTTP.Response ())
httpHeadRequest url = do
  req <-
    HTTP.setRequestIgnoreStatus
      . HTTP.setRequestMethod "HEAD"
      <$> HTTP.parseRequest url

  HTTP.httpNoBody req
