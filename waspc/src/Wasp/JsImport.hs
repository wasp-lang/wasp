module Wasp.JsImport
    ( JsImport(..)
    ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Path.Posix as PPosix


-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
    { _defaultImport :: !(Maybe String)
    , _namedImports  :: ![String]
    -- Relative to source external code dir.
    , _from          :: !(PPosix.Path PPosix.Rel PPosix.File)
    } deriving (Show, Eq)

instance ToJSON JsImport where
    toJSON jsImport = object
        [ "defaultImport" .= _defaultImport jsImport
        , "namedImports" .= _namedImports jsImport
        , "from" .= PPosix.toFilePath (_from jsImport)
        ]
