module Wasp.Wasp.Server
  ( Server (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Wasp.Wasp.JsImport (JsImport)

newtype Server = Server
  { _setupJsFunction :: JsImport
  }
  deriving (Show, Eq)

instance ToJSON Server where
  toJSON server =
    object
      [ "setupJsFunction" .= _setupJsFunction server
      ]
