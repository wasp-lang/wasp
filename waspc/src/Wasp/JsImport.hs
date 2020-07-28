module Wasp.JsImport
    ( JsImport(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))

import StrongPath (Path, Rel, File)
import qualified StrongPath as SP
import ExternalCode (SourceExternalCodeDir)


-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
    { jsImportWhat :: !String
    , jsImportFrom :: !(Path (Rel SourceExternalCodeDir) File)
    } deriving (Show, Eq)

instance ToJSON JsImport where
    toJSON jsImport = object
        [ "what" .= jsImportWhat jsImport
        , "from" .= (SP.toFilePath $ jsImportFrom jsImport)
        ]
