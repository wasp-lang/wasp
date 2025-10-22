{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.News
  ( news,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), decode, genericParseJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as L
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command)

{-# NOINLINE waspNewsServerUrl #-}
waspNewsServerUrl :: String
waspNewsServerUrl =
  fromMaybe "https://news.wasp.sh" $ unsafePerformIO $ lookupEnv "WASP_NEWS_SERVER_URL"

data WaspNewsEntry = WaspNewsEntry
  { -- _wneId :: !String,
    _wneTitle :: !String,
    _wneBody :: !String,
    _wnePublishedAt :: !String
  }
  deriving (Generic, Show)

instance FromJSON WaspNewsEntry where
  parseJSON =
    genericParseJSON $
      Aeson.defaultOptions {Aeson.fieldLabelModifier = modifyFieldLabel}
    where
      -- modifyFieldLabel "_wneId" = "id"
      modifyFieldLabel "_wneTitle" = "title"
      modifyFieldLabel "_wneBody" = "body"
      modifyFieldLabel "_wnePublishedAt" = "publishedAt"
      modifyFieldLabel other = other

news :: Command ()
news = do
  liftIO $ do
    putStrLn ""
    printNewsHeader

    response <- httpBS =<< parseRequest waspNewsServerUrl
    let responseBody = L.fromStrict $ getResponseBody response
    case decode responseBody of
      Nothing -> putStrLn "Failed to parse news data"
      Just (newsEntries :: [WaspNewsEntry]) -> mapM_ printNewsEntry newsEntries

printNewsHeader :: IO ()
printNewsHeader = do
  putStrLn "      ┌─────────────────────┐"
  putStrLn "      │   📰 WASP NEWS 📰   │"
  putStrLn "      └─────────────────────┘"

printNewsEntry :: WaspNewsEntry -> IO ()
printNewsEntry entry = do
  putStrLn ""
  putStrLn $ " 📌 " <> _wneTitle entry <> " • [" <> _wnePublishedAt entry <> "]"
  putStrLn ""
  putStrLn $ "    " <> _wneBody entry
  putStrLn ""
  putStrLn "─────────────────────────────────────────────────────────"
