module Wasp.Cli.Command.News
  ( news,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command)

{-# NOINLINE waspNewsServerUrl #-}
waspNewsServerUrl :: String
waspNewsServerUrl =
  fromMaybe "https://news.wasp.sh" $ unsafePerformIO $ lookupEnv "WASP_NEWS_SERVER_URL"

news :: Command ()
news = do
  liftIO $ do
    putStrLn "  WASP NEWS  "
    putStrLn "============="

    response <- httpBS =<< parseRequest waspNewsServerUrl
    let newsJsonStr = show $ getResponseBody response
    print newsJsonStr
