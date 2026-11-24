module Wasp.Util.IO.Process
  ( OutputStream (..),
    WriteOutput,
  )
where

import Data.Text (Text)

-- | One of the two streams that a program writes its output to.
data OutputStream = Stdout | Stderr deriving (Show, Eq, Ord)

-- | Writes text out to one of the output streams.
type WriteOutput = OutputStream -> Text -> IO ()
