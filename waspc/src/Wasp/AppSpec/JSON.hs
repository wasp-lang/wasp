{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
  )
where

import qualified Data.Aeson as Aeson
import Data.Data (Data)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)

newtype JSON = JSON Aeson.Value
  deriving (Eq, Data)

instance Show JSON where
  show (JSON val) = unpack $ decodeUtf8 $ Aeson.encode val
