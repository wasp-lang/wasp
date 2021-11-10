module Wasp.Wasp.App
  ( App (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))

data App = App
  { appName :: !String, -- Identifier
    appTitle :: !String,
    appHead :: !(Maybe [String])
  }
  deriving (Show, Eq)

instance ToJSON App where
  toJSON app =
    object
      [ "name" .= appName app,
        "title" .= appTitle app
      ]
