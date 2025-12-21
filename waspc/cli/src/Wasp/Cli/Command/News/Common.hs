{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.News.Common
  ( NewsEntry (..),
    NewsLevel (..),
    debug,
  )
where

import Data.Aeson (FromJSON (..), genericParseJSON)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Time (UTCTime)
import GHC.Generics

data NewsLevel
  = Low
  | Moderate
  | High
  deriving (Eq, Ord, Show, Generic)

instance FromJSON NewsLevel where
  parseJSON = genericParseJSON $ Aeson.defaultOptions {Aeson.constructorTagModifier = map toLower}

data NewsEntry = NewsEntry
  { id :: !String,
    title :: !String,
    body :: !String,
    level :: !NewsLevel,
    publishedAt :: !UTCTime
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewsEntry

debug :: String -> IO ()
debug message = print message
