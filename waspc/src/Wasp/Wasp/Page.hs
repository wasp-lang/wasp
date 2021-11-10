module Wasp.Wasp.Page
  ( Page (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import Wasp.Wasp.JsImport (JsImport)

data Page = Page
  { _name :: !String,
    _component :: !JsImport,
    _authRequired :: Maybe Bool
  }
  deriving (Show, Eq)

instance ToJSON Page where
  toJSON page =
    object
      [ "name" .= _name page,
        "component" .= _component page
      ]
