{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
    emptyObject,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.UTF8 as ByteStringLazyUTF8
import Data.Data (Data)

newtype JSON = JSON Aeson.Value
  deriving (Eq, Data)

instance Show JSON where
  show (JSON val) = ByteStringLazyUTF8.toString $ Aeson.encode val

emptyObject :: JSON
emptyObject = JSON $ Aeson.object []
