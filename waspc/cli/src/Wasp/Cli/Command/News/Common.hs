{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.News.Common
  ( NewsEntry (..),
    NewsLevel (..),
  )
where

import Data.Aeson (FromJSON (..), genericParseJSON)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data NewsEntry = NewsEntry
  { id :: !String,
    title :: !String,
    body :: !String,
    level :: !NewsLevel,
    publishedAt :: !UTCTime
  }
  deriving (Generic, Show, Eq)

instance FromJSON NewsEntry

data NewsLevel
  = Low
  | Moderate
  | High
  deriving (Eq, Ord, Show, Generic)

instance FromJSON NewsLevel where
  parseJSON = genericParseJSON $ Aeson.defaultOptions {Aeson.constructorTagModifier = map toLower}
