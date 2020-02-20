module Wasp.App
    ( App(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))


data App = App
    { appName :: !String -- Identifier
    , appTitle :: !String
    } deriving (Show, Eq)

instance ToJSON App where
    toJSON app = object
        [ "name" .= appName app
        , "title" .= appTitle app
        ]
