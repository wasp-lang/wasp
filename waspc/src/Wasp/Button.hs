module Wasp.Button
    ( Button(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

data Button = Button
    { _name :: !String
    , _label :: !String
    , _onClickActionName :: !(Maybe String)
    } deriving (Show, Eq)

instance ToJSON Button where
    toJSON button = object
        [ "name" .= _name button
        , "label" .= _label button
        , "onClickActionName" .= _onClickActionName button
        ]
