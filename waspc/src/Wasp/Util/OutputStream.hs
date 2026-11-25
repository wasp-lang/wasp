module Wasp.Util.OutputStream
  ( OutputStream (..),
  )
where

-- | One of the two streams that a program writes its output to.
data OutputStream = Stdout | Stderr deriving (Show, Eq, Ord)
