module Wasp.Analyzer.Parser.SourceRegion
  ( SourceRegion (..),
    getRgnStart,
    getRgnEnd,
    sourceSpanToRegion,
  )
where

import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..), sourceOffsetToPosition)
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (..))

-- | @SourceRegion <regionStart> <regionEnd>@
-- where @regionStart@ is position of the first character in the region while @regionEnd@
-- is the position of the last character in the region.
-- So, if we have `app MyApp {` and we want to capture the `MyApp`,
-- we would do it with @SourceRegion (SourcePosition 1 5) (SourcePosition 1 9)@.
data SourceRegion = SourceRegion SourcePosition SourcePosition
  deriving (Eq)

instance Show SourceRegion where
  show (SourceRegion startPos@(SourcePosition startLine _startCol) endPos@(SourcePosition endLine endCol))
    | startPos == endPos = show startPos
    | startLine == endLine = show startPos ++ "-" ++ show endCol
    | otherwise = show startPos ++ "-" ++ show endPos

getRgnStart :: SourceRegion -> SourcePosition
getRgnStart (SourceRegion start _) = start

getRgnEnd :: SourceRegion -> SourcePosition
getRgnEnd (SourceRegion _ end) = end

sourceSpanToRegion :: String -> SourceSpan -> SourceRegion
sourceSpanToRegion source (SourceSpan start end) =
  let startPos = sourceOffsetToPosition source start
      endPos = sourceOffsetToPosition source (end - 1)
   in SourceRegion startPos endPos
