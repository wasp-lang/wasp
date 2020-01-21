module Wasp.JsImport
    ( JsImport(..)
    ) where

import Data.Aeson ((.=), object, ToJSON(..))


-- | Represents javascript import -> "import <what> from <from>".
data JsImport = JsImport
    { jsImportWhat :: !String
    -- | Path of file to import, relative to external code directory.
    --   So for example if jsImportFrom is "test.js", we expect file
    --   to exist at <external_code_dir>/test.js.
    --   TODO: Make this more explicit in the code (both here and in wasp lang)? Also, support importing npm packages?
    --   TODO: Use Path.RelFile here? Maybe that would be too restrictive, since we might even have
    --     even have things here that are not filepath, so let's see in the future?
    --     I tried, but I did not know how to easily call Path.parseRelFile inside parsec, so I gave up for now.
    , jsImportFrom :: !FilePath
    } deriving (Show, Eq)

instance ToJSON JsImport where
    toJSON jsImport = object
        [ "what" .= jsImportWhat jsImport
        , "from" .= jsImportFrom jsImport
        ]
