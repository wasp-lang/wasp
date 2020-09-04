module Wasp.Action
    ( Action(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Wasp.JsImport (JsImport)

data Action = Action
    { _name :: !String
    , _jsFunction :: !JsImport
    } deriving (Show, Eq)

instance ToJSON Action where
    toJSON action = object
        [ "name" .= _name action
        , "jsFunction" .= _jsFunction action
        ]
