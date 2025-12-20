{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.News.Common
  ( NewsEntry (..),
    debug,
  )
where

import Data.Aeson (FromJSON)
import Data.Time (UTCTime)
import GHC.Generics

data NewsEntry = NewsEntry
  { id :: !String,
    title :: !String,
    body :: !String,
    level :: !String,
    publishedAt :: !UTCTime
  }
  deriving (Generic, Show)

instance FromJSON NewsEntry

debug :: String -> IO ()
debug message = print message
