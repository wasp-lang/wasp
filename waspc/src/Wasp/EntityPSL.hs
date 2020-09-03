-- TODO(matija): remove PSL suffix, added it to avoid clashes with the existing Entity module.
-- Once the old Entity module is removed, rename to "Entity".
module Wasp.EntityPSL
    ( EntityPSL (..)
    ) where

data EntityPSL = EntityPSL
    { _name :: !String
    , _pslModelSchema :: !String
    } deriving (Show, Eq)
