module Wasp.Analyzer.Parser.SourceRegion
  ( SourceRegion (..),
    getRgnStart,
    getRgnEnd,
  )
where

import Wasp.Analyzer.Parser.SourcePosition

-- | @SourceRegion <regionStart> <regionEnd>@
-- where @regionStart@ is position of the first character in the region while @regionEnd@
-- is the position of the last character in the region.
-- So, if we have `app MyApp {` and we want to capture the `MyApp`,
-- we would do it with @SourceRegion (SourcePosition 1 5) (SourcePosition 1 9)@.
data SourceRegion = SourceRegion SourcePosition SourcePosition
  deriving (Eq, Show)

getRgnStart :: SourceRegion -> SourcePosition
getRgnStart (SourceRegion start _) = start

getRgnEnd :: SourceRegion -> SourcePosition
getRgnEnd (SourceRegion _ end) = end
