module Wasp.Page
    ( Page(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))
import Data.Text (Text)

data Page = Page
    { pageName :: !String
    , pageRoute :: !String
    , pageContent :: !String
    -- | TODO(martin): I did not know how to apply strictness annotation (!) here.
    , pageStyle :: Maybe Text
    } deriving (Show, Eq)

instance ToJSON Page where
    toJSON page = object
        [ "name" .= pageName page
        , "route" .= pageRoute page
        , "content" .= pageContent page
        ]
