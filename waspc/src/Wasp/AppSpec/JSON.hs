{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
  )
where

import Data.Data (Data)

-- TOOD: In future we should convert this to Aeson.
newtype JSON = JSON String
  deriving (Eq, Data)

-- TODO: We should fix implicit braces syntax.
instance Show JSON where
  show (JSON str) = "{" ++ str ++ "}"
