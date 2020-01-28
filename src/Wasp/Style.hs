module Wasp.Style
    ( Style(..)
    ) where

import Data.Aeson (ToJSON(..))
import Data.Text (Text)
import qualified Path.Aliases as Path


data Style = ExtCodeCssFile !Path.RelFile
           | CssCode !Text
    deriving (Show, Eq)

instance ToJSON Style where
    toJSON (ExtCodeCssFile path) = toJSON path
    toJSON (CssCode code) = toJSON code
