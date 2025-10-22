{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
    emptyObject,
    nullValue,
    maybeToField,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Text as Aeson.Text
import qualified Data.Aeson.Types as Aeson.Types
import Data.Data (Data)
import Data.Text (pack)
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

-- | Helper function to convert a Maybe value to an optional JSON field pair.
maybeToField :: Aeson.ToJSON a => String -> Maybe a -> Maybe Aeson.Types.Pair
maybeToField _ Nothing = Nothing
maybeToField key (Just val) = Just (pack key Aeson..= val)
