module Wasp.Server
  ( Server (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Wasp.JsImport (JsImport)

data Server = Server
  { _setupJsFunction :: !JsImport
  }
  deriving (Show, Eq)

instance ToJSON Server where
  toJSON server =
    object
      [ "setupJsFunction" .= _setupJsFunction server
      ]
