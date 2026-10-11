-- | Working out which part of a pane's logs is on screen.
--
-- The log body is laid out by hand rather than with a brick viewport, because
-- scrolling has to be counted in wrapped display rows: one log line can occupy
-- several rows, and "scroll up by one" has to mean one row, not one line.
module Wasp.Cli.Tui.Layout
  ( LogRow (..),
    logGutterWidth,
    logContentWidthFor,
    visibleLogRows,
    wrapLogLine,
    wrapSegments,
  )
where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Wasp.Cli.Tui.Ansi (Segment (..))
import Wasp.Cli.Tui.LogBuffer (LogLine (..), bufferedLines)
import Wasp.Cli.Tui.Pane (Pane (..))

-- | Columns taken up by the colored bar to the left of every log row.
logGutterWidth :: Int
logGutterWidth = 2

-- | How wide log text may be in a terminal of the given width.
logContentWidthFor :: Int -> Int
logContentWidthFor terminalWidth = max 1 (terminalWidth - logGutterWidth)

-- | One display row: a wrapped piece of a log line, together with the line it
-- came from, which the renderer needs to style it.
data LogRow = LogRow
  { logRowSegments :: ![Segment],
    logRowLine :: !LogLine
  }

-- | The rows of a pane that fit on a screen of the given width and height,
-- top row first, along with how many rows are hidden below them.
--
-- The hidden count is the scroll position after clamping: scrolling further
-- back than the oldest line just parks the view at the top.
visibleLogRows :: Int -> Int -> Pane -> ([LogRow], Int)
visibleLogRows width height pane =
  (reverse $ take visibleHeight $ drop clampedOffset rowsNewestFirst, clampedOffset)
  where
    contentWidth = max 1 width
    visibleHeight = max 1 height

    linesNewestFirst = toList $ Seq.reverse $ bufferedLines (paneBuffer pane)

    -- Lazy on purpose: wrapping stops as soon as there are enough rows to fill
    -- the screen, so a pane with thousands of buffered lines costs no more to
    -- draw than one with a screenful.
    rowsNewestFirst =
      take (requestedOffset + visibleHeight) $
        concatMap (reverse . wrapLogLine contentWidth) linesNewestFirst

    requestedOffset = if paneIsFollowing pane then 0 else paneScrollOffset pane
    clampedOffset = min requestedOffset (max 0 (length rowsNewestFirst - visibleHeight))

-- | Splits one log line into the display rows it occupies, top row first.
wrapLogLine :: Int -> LogLine -> [LogRow]
wrapLogLine width line =
  map (`LogRow` line) (wrapSegments width (map expandTabs (logLineSegments line)))
  where
    -- A raw tab would make the renderer and our own width arithmetic disagree.
    expandTabs segment = segment {segmentText = T.replace "\t" "    " (segmentText segment)}

-- | Wraps a line's styled segments into rows at most @width@ columns wide.
-- Always returns at least one row, so that an empty line still takes up a row.
wrapSegments :: Int -> [Segment] -> [[Segment]]
wrapSegments width segments = case fillRow width [] segments of
  ([], []) -> [[]]
  (lastRow, fullRows) -> reverse (reverse lastRow : fullRows)
  where
    -- Walks the segments filling one row at a time. 'remaining' is how many
    -- columns are left in the row being filled and 'current' holds its
    -- segments in reverse. Returns the unfinished last row (still reversed)
    -- and the rows that filled up, newest first.
    fillRow _ current [] = (current, [])
    fillRow remaining current (segment : rest)
      | T.null (segmentText segment) = fillRow remaining current rest
      | T.length (segmentText segment) <= remaining =
          fillRow (remaining - T.length (segmentText segment)) (segment : current) rest
      | otherwise =
          let (fits, overflow) = T.splitAt remaining (segmentText segment)
              filledRow = reverse (segment {segmentText = fits} : current)
              (lastRow, fullRows) = fillRow width [] (segment {segmentText = overflow} : rest)
           in (lastRow, fullRows ++ [filledRow])
