module Wasp.JsImport
  ( JsImport (..),
  )
where

import Data.Aeson (ToJSON (..), object, (.=))
import ExternalCode (SourceExternalCodeDir)
import StrongPath (File, Path', Posix, Rel)
import qualified StrongPath as SP

-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
  { _defaultImport :: !(Maybe String),
    _namedImports :: ![String],
    _from :: Path' Posix (Rel SourceExternalCodeDir) File
  }
  deriving (Show, Eq)

instance ToJSON JsImport where
  toJSON jsImport =
    object
      [ "defaultImport" .= _defaultImport jsImport,
        "namedImports" .= _namedImports jsImport,
        "from" .= SP.toFilePath (_from jsImport)
      ]
