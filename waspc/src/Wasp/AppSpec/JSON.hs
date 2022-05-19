{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
    emptyObject,
    nullValue,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Aeson.Types as Aeson.Types
import Data.Data (Data)
import qualified Data.Text.Lazy as TextL

newtype JSON = JSON Aeson.Value
  deriving (Eq, Data)

instance Show JSON where
  show (JSON val) = TextL.unpack . Aeson.Text.encodeToLazyText $ val

instance Aeson.ToJSON JSON where
  toJSON (JSON val) = val

instance Aeson.FromJSON JSON where
  parseJSON val = return $ JSON val

emptyObject :: JSON
emptyObject = JSON Aeson.Types.emptyObject

nullValue :: JSON
nullValue = JSON Aeson.Types.Null
