-- | A bounded, append-only buffer of log lines for one TUI pane.
--
-- Processes write their output in arbitrary chunks that rarely line up with
-- line boundaries, so the buffer keeps a partial line per stream (stdout and
-- stderr separately, since the two interleave) and only commits a line once
-- its newline arrives.
module Wasp.Cli.Tui.LogBuffer
  ( LogBuffer,
    LogLine (..),
    LogLevel (..),
    emptyLogBuffer,
    appendChunk,
    bufferedLines,
    numBufferedLines,
    numDroppedLines,
    clearLogBuffer,
  )
where

import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Wasp.Cli.Tui.Ansi (Segment (..), parseAnsi, segmentsHaveColor, segmentsToText)

-- | How important a line looks, guessed from its content. Drives the color we
-- give lines that don't color themselves.
data LogLevel
  = LevelNormal
  | LevelInfo
  | LevelSuccess
  | LevelWarn
  | LevelError
  deriving (Eq, Ord, Show)

data LogLine = LogLine
  { logLineSegments :: ![Segment],
    logLineText :: !T.Text,
    logLineLevel :: !LogLevel,
    logLineIsStderr :: !Bool,
    -- | Whether the line colors itself with ANSI codes, in which case we
    -- render it as-is instead of applying the level color.
    logLineIsSelfColored :: !Bool
  }

-- | A line that has been started but whose newline hasn't arrived yet.
data PartialLine = PartialLine
  { partialSegments :: ![Segment],
    -- | Attribute the next chunk of this stream continues with.
    partialAttr :: !V.Attr
  }

emptyPartialLine :: PartialLine
emptyPartialLine = PartialLine {partialSegments = [], partialAttr = V.defAttr}

data LogBuffer = LogBuffer
  { bufferLines :: !(Seq LogLine),
    bufferNumDropped :: !Int,
    bufferStdout :: !PartialLine,
    bufferStderr :: !PartialLine
  }

-- | How many lines we keep per pane. Dev servers can be chatty, and we'd
-- rather bound memory than keep every line of a session that runs for days.
maxBufferedLines :: Int
maxBufferedLines = 5000

emptyLogBuffer :: LogBuffer
emptyLogBuffer =
  LogBuffer
    { bufferLines = Seq.empty,
      bufferNumDropped = 0,
      bufferStdout = emptyPartialLine,
      bufferStderr = emptyPartialLine
    }

bufferedLines :: LogBuffer -> Seq LogLine
bufferedLines = bufferLines

numBufferedLines :: LogBuffer -> Int
numBufferedLines = Seq.length . bufferLines

numDroppedLines :: LogBuffer -> Int
numDroppedLines = bufferNumDropped

-- | Clears the lines but keeps the partial-line state, so a line that is
-- mid-flight when the user clears the pane still arrives intact.
clearLogBuffer :: LogBuffer -> LogBuffer
clearLogBuffer buffer = buffer {bufferLines = Seq.empty, bufferNumDropped = 0}

-- | Appends a chunk of output, returning the updated buffer along with the
-- lines that the chunk completed (callers use those to update status and
-- error counts).
appendChunk :: Bool -> T.Text -> LogBuffer -> (LogBuffer, [LogLine])
appendChunk isStderr chunk buffer = (buffer', reverse newLinesReversed)
  where
    (buffer', newLinesReversed) = foldl' step (buffer, []) (zip [0 :: Int ..] pieces)

    -- Normalizing CRLF first means the remaining \r are true carriage
    -- returns: progress-bar style redraws of the current line.
    pieces = T.splitOn "\n" $ T.replace "\r\n" "\n" chunk

    step (buf, newLines) (index, piece) =
      let buf' = feedPiece isStderr piece buf
       in -- Every piece but the last one was terminated by a newline.
          if index < length pieces - 1
            then
              let (buf'', line) = commitPartialLine isStderr buf'
               in (buf'', line : newLines)
            else (buf', newLines)

-- | Adds a piece of a single line to the matching partial line. A carriage
-- return inside the piece discards whatever was written to the line so far,
-- which is how terminals handle the in-place redraws of progress output.
feedPiece :: Bool -> T.Text -> LogBuffer -> LogBuffer
feedPiece isStderr piece buffer =
  setPartialLine isStderr (foldl' addPart (getPartialLine isStderr buffer) (zip [0 :: Int ..] (T.splitOn "\r" piece))) buffer
  where
    addPart partial (index, part) =
      let partial' = if index > 0 then partial {partialSegments = []} else partial
          (attr', segments) = parseAnsi (partialAttr partial') part
       in PartialLine
            { partialSegments = partialSegments partial' ++ segments,
              partialAttr = attr'
            }

commitPartialLine :: Bool -> LogBuffer -> (LogBuffer, LogLine)
commitPartialLine isStderr buffer = (buffer', line)
  where
    partial = getPartialLine isStderr buffer
    segments = partialSegments partial
    text = segmentsToText segments
    line =
      LogLine
        { logLineSegments = segments,
          logLineText = text,
          logLineLevel = detectLogLevel text,
          logLineIsStderr = isStderr,
          logLineIsSelfColored = segmentsHaveColor segments
        }

    -- The attribute carries over to the next line: a process that sets a color
    -- and then prints several lines expects all of them to be colored.
    buffer' =
      pushLine line $
        setPartialLine isStderr partial {partialSegments = []} buffer

pushLine :: LogLine -> LogBuffer -> LogBuffer
pushLine line buffer
  | Seq.length lines' > maxBufferedLines =
      buffer
        { bufferLines = Seq.drop (Seq.length lines' - maxBufferedLines) lines',
          bufferNumDropped = bufferNumDropped buffer + (Seq.length lines' - maxBufferedLines)
        }
  | otherwise = buffer {bufferLines = lines'}
  where
    lines' = bufferLines buffer |> line

getPartialLine :: Bool -> LogBuffer -> PartialLine
getPartialLine isStderr buffer = if isStderr then bufferStderr buffer else bufferStdout buffer

setPartialLine :: Bool -> PartialLine -> LogBuffer -> LogBuffer
setPartialLine isStderr partial buffer =
  if isStderr then buffer {bufferStderr = partial} else buffer {bufferStdout = partial}

-- | Guesses a line's level from its text. Deliberately conservative: we'd
-- rather leave a line uncolored than paint an innocent line red.
detectLogLevel :: T.Text -> LogLevel
detectLogLevel text
  | mentions errorWords && not (mentions noErrorWords) = LevelError
  | mentions warningWords = LevelWarn
  | mentions successWords = LevelSuccess
  | otherwise = LevelNormal
  where
    lowercased = T.toLower text
    mentions = any (`T.isInfixOf` lowercased)

    errorWords =
      ["error", "err!", "failed", "failure", "fatal", "exception", "cannot find", "✘", "✗"]
    -- Build tools love to announce the absence of errors, and we don't want
    -- "0 errors" to light up the pane.
    noErrorWords = ["0 error", "no error", "errors: 0", "error-free"]
    warningWords = ["warn", "deprecat", "⚠"]
    successWords =
      ["ready in", "listening on", "compiled successfully", "successfully", "✓", "✔", "ready!"]
