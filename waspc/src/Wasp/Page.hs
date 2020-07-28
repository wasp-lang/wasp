module Wasp.Page
    ( Page(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

import qualified Wasp.Style as WStyle


data Page = Page
    { pageName :: !String
    , pageContent :: !String
    -- | TODO(martin): I did not know how to apply strictness annotation (!) here.
    , pageStyle :: Maybe WStyle.Style
    } deriving (Show, Eq)

instance ToJSON Page where
    toJSON page = object
        [ "name" .= pageName page
        , "content" .= pageContent page
        , "style" .= pageStyle page
        ]
