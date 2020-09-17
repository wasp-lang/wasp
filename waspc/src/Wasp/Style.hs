module Wasp.Style
    ( Style(..)
    ) where

import Data.Aeson (ToJSON(..))
import Data.Text (Text)
import qualified Path.Posix as PPosix


data Style = ExtCodeCssFile !(PPosix.Path PPosix.Rel PPosix.File)
           | CssCode !Text
    deriving (Show, Eq)

instance ToJSON Style where
    toJSON (ExtCodeCssFile path) = toJSON $ PPosix.toFilePath path
    toJSON (CssCode code) = toJSON code
