{-# LANGUAGE DeriveDataTypeable #-}

module Wasp.AppSpec.JSON
  ( JSON (..),
    waspJSONtoString,
  )
where

import Data.Data (Data)

-- TOOD: In future we should convert this to Aeson.
newtype JSON = JSON String
  deriving (Show, Eq, Data)

-- TODO: We should fix implicit braces syntax.
waspJSONtoString :: JSON -> String
waspJSONtoString (JSON str) = "{" ++ str ++ "}"
