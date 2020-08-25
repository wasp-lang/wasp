module Wasp.Query
    ( Query(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Wasp.JsImport (JsImport)

data Query = Query
    { _name :: !String
    , _jsFunction :: !JsImport
    } deriving (Show, Eq)

instance ToJSON Query where
    toJSON query = object
        [ "name" .= _name query
        , "jsFunction" .= _jsFunction query
        ]
