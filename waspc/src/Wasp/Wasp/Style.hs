module Wasp.Wasp.Style
  ( Style (..),
  )
where

import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Data.Aeson (ToJSON (..))
import Data.Text (Text)
import StrongPath (File', Path, Posix, Rel, toFilePath)

data Style
  = ExtCodeCssFile !(Path Posix (Rel SourceExternalCodeDir) File')
  | CssCode !Text
  deriving (Show, Eq)

instance ToJSON Style where
  toJSON (ExtCodeCssFile path) = toJSON $ toFilePath path
  toJSON (CssCode code) = toJSON code
