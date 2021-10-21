module Wasp.Ref
  ( Ref (..),
  )
where

-- | Reference to a named Wasp declaration.
newtype Ref a = Ref {unref :: String}
