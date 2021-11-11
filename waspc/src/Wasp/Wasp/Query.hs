module Wasp.Wasp.Query
  ( Query (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Wasp.Wasp.JsImport (JsImport)

-- TODO: Very similar to Wasp.Action, consider extracting duplication.

data Query = Query
  { _name :: !String,
    _jsFunction :: !JsImport,
    _entities :: !(Maybe [String]),
    _auth :: !(Maybe Bool)
  }
  deriving (Show, Eq)

instance ToJSON Query where
  toJSON query =
    object
      [ "name" .= _name query,
        "jsFunction" .= _jsFunction query,
        "entities" .= _entities query,
        "auth" .= _auth query
      ]
