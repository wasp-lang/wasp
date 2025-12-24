module Wasp.Cli.Util.Http (checkUrlExists) where

import qualified Network.HTTP.Conduit as H
import Network.HTTP.Simple (httpNoBody)
import qualified Network.HTTP.Simple as H
import Network.HTTP.Types.Status (statusIsSuccessful)

checkUrlExists :: String -> IO Bool
checkUrlExists url = do
  res <- httpHeadRequest url
  return $ statusIsSuccessful $ H.getResponseStatus res

httpHeadRequest :: String -> IO (H.Response ())
httpHeadRequest url = do
  req <-
    H.setRequestIgnoreStatus
      . H.setRequestMethod "HEAD"
      <$> H.parseUrlThrow url

  httpNoBody req
