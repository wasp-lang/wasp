module Wasp.Style
    ( Style(..)
    ) where

import Data.Aeson (ToJSON(..))
import Data.Text (Text)

import StrongPath (Path, Rel, File)
import qualified StrongPath as SP
import ExternalCode (SourceExternalCodeDir)


data Style = ExtCodeCssFile !(Path (Rel SourceExternalCodeDir) File)
           | CssCode !Text
    deriving (Show, Eq)

instance ToJSON Style where
    toJSON (ExtCodeCssFile path) = toJSON $ SP.toFilePath path
    toJSON (CssCode code) = toJSON code
