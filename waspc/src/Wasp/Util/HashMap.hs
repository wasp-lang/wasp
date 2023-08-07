module Wasp.Util.HashMap
  ( lookupKey,
  )
where

import Data.Foldable (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M

-- | Lookup a key, returning the key stored in the map. Useful when the 'Eq'
-- instance on the key type isn't structural equality.
--
-- === __Example__
-- >>> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- >>> import Data.Hashable (Hashable)
-- >>> newtype ApproxDouble = ApproxDouble Double deriving (Show, Hashable)
-- >>> instance Eq ApproxDouble where
-- >>>   ApproxDouble x == ApproxDouble y = abs (x - y) < 0.01
-- >>> lookupKey (ApproxDouble 0.502) $ M.fromList [(ApproxDouble 0.5, "a"), (ApproxDouble 0.6, "b")]
-- 0.5
lookupKey :: (Eq k) => k -> HashMap k v -> Maybe k
lookupKey k = find (== k) . M.keys
