-- TODO(matija): remove PSL suffix, added it to avoid clashes with the existing Entity module.
-- Once the old Entity module is removed, rename to "Entity".
module Wasp.EntityPSL
    ( EntityPSL (..)
    ) where

import Data.Text (Text)

data EntityPSL = EntityPSL
    { _name :: !String
    , _pslModelSchema :: !Text -- ^ PSL stands for Prisma Schema Language.
    } deriving (Show, Eq)
