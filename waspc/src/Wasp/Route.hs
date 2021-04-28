module Wasp.Route
  ( Route (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))

data Route = Route
  { _urlPath :: !String,
    -- NOTE(matija): for now page is the only possible target, but in
    -- the future there might be different types of targets (e.g. another route).
    _targetPage :: !String
  }
  deriving (Show, Eq)

instance ToJSON Route where
  toJSON route =
    object
      [ "urlPath" .= _urlPath route,
        "targetPage" .= _targetPage route
      ]
