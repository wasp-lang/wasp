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
    displayWidth,
  )
where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Graphics.Vty as V
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
    logRowLine :: !LogLine,
    -- | Whether this is the first row of its log line.
    --
    -- Continuation rows are drawn without the gutter bar: the terminal has no
    -- idea the rows belong together, so a selection dragged across a wrap
    -- would otherwise pick the gutter up into the middle of the copied text.
    -- That is how a wrapped URL turns into an unusable one.
    logRowIsLineStart :: !Bool
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
  zipWith
    (\isLineStart rowSegments -> LogRow rowSegments line isLineStart)
    (True : repeat False)
    (wrapSegments width (map expandTabs (logLineSegments line)))
  where
    -- A raw tab would make the renderer and our own width arithmetic disagree.
    expandTabs segment = segment {segmentText = T.replace "\t" "    " (segmentText segment)}

-- | How many terminal columns a piece of text occupies.
--
-- Not the same as how many characters it has: an emoji or a CJK glyph takes
-- two columns and a combining mark takes none. Vty measures the images it
-- draws this way, so the wrapping has to agree with it, or a row it believes
-- fits gets drawn wider than the pane and silently cropped. Wasp's own server
-- output has emoji in it, so this is not hypothetical.
displayWidth :: T.Text -> Int
displayWidth = V.safeWcswidth . T.unpack

-- | Splits off the longest prefix that fits in the given number of columns.
splitAtDisplayWidth :: Int -> T.Text -> (T.Text, T.Text)
splitAtDisplayWidth maxWidth text = T.splitAt numCharsThatFit text
  where
    numCharsThatFit = countCharsThatFit 0 0 text
    countCharsThatFit numChars usedWidth remaining = case T.uncons remaining of
      Nothing -> numChars
      Just (character, rest)
        | usedWidth + V.safeWcwidth character > maxWidth -> numChars
        | otherwise ->
            countCharsThatFit (numChars + 1) (usedWidth + V.safeWcwidth character) rest

-- | Wraps a line's styled segments into rows at most @width@ columns wide.
-- Always returns at least one row, so that an empty line still takes up a row.
wrapSegments :: Int -> [Segment] -> [[Segment]]
wrapSegments width segments = case fillRow width [] segments of
  ([], []) -> [[]]
  -- The text ended exactly on a row boundary, so there is no partial row to
  -- add. Adding one anyway would draw a blank row after the line.
  ([], fullRows) -> reverse fullRows
  (lastRow, fullRows) -> reverse (reverse lastRow : fullRows)
  where
    -- Walks the segments filling one row at a time. 'remaining' is how many
    -- columns are left in the row being filled and 'current' holds its
    -- segments in reverse. Returns the unfinished last row (still reversed)
    -- and the rows that filled up, newest first.
    fillRow _ current [] = (current, [])
    fillRow remaining current (segment : rest)
      | T.null (segmentText segment) = fillRow remaining current rest
      | displayWidth (segmentText segment) <= remaining =
          fillRow (remaining - displayWidth (segmentText segment)) (segment : current) rest
      | otherwise =
          let (fits, overflow) = splitAtDisplayWidth remaining (segmentText segment)
              (fitsOrFirstCharacter, remainder)
                | not (T.null fits) = (fits, overflow)
                -- A character too wide for an empty row fits nowhere, so take
                -- it anyway: it gets cropped, but the wrapping terminates.
                | null current = T.splitAt 1 (segmentText segment)
                -- Nothing more fits on this row but it isn't empty, so flush
                -- it and let the character start the next one.
                | otherwise = (T.empty, segmentText segment)
              filledRow = reverse (segment {segmentText = fitsOrFirstCharacter} : current)
              (lastRow, fullRows) = fillRow width [] (segment {segmentText = remainder} : rest)
           in (lastRow, fullRows ++ [filledRow])
