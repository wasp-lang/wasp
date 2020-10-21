-- TODO(matija): remove PSL suffix, added it to avoid clashes with the existing Entity module.
-- Once the old Entity module is removed, rename to "Entity".
module Wasp.Entity
    ( Entity (..)
    ) where

import Data.Text (Text)
import Data.Aeson (ToJSON(..), (.=), object)

data Entity = Entity
    { _name :: !String
    , _pslModelSchema :: !Text -- ^ PSL stands for Prisma Schema Language.
    } deriving (Show, Eq)

instance ToJSON Entity where
    toJSON entity = object
        [ "name" .= _name entity
        , "pslModelSchema" .= _pslModelSchema entity
        ]
