module Wasp.JsImport
    ( JsImport(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

import StrongPath (Path, Rel, File)
import qualified StrongPath as SP
import ExternalCode (SourceExternalCodeDir)


-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
    { _defaultImport :: !(Maybe String)
    , _namedImports :: ![String]
    , _from :: !(Path (Rel SourceExternalCodeDir) File)
    } deriving (Show, Eq)

instance ToJSON JsImport where
    toJSON jsImport = object
        [ "defaultImport" .= _defaultImport jsImport
        , "namedImports" .= _namedImports jsImport
        , "from" .= SP.toFilePath (_from jsImport)
        ]
