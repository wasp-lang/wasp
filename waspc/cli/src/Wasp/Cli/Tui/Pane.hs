-- | A single tab of the @wasp start@ TUI: the logs of one process, plus
-- everything we show about it in the tab bar.
module Wasp.Cli.Tui.Pane
  ( PaneId (..),
    Pane (..),
    PaneStatus (..),
    paneIdOfJobType,
    makePane,
    feedPane,
    markPaneExited,
    clearPane,
    scrollPaneBy,
    scrollPaneToTop,
    scrollPaneToBottom,
    setPaneScrollOffset,
    togglePaneFollow,
    markPaneSeen,
    paneStatusLabel,
    paneIsBusy,
  )
where

import Data.List (foldl')
import qualified Data.Text as T
import qualified Graphics.Vty as V
import System.Exit (ExitCode (..))
import Wasp.Cli.Tui.LogBuffer
  ( LogBuffer,
    LogLevel (..),
    LogLine (..),
    appendChunk,
    clearLogBuffer,
    emptyLogBuffer,
    maxBufferedLinesPerPane,
  )
import qualified Wasp.Cli.Tui.Theme as Theme
import qualified Wasp.Job as J

-- | The panes, in the order they appear in the tab bar.
data PaneId = PaneWasp | PaneServer | PaneClient | PaneDb
  deriving (Eq, Ord, Show, Enum, Bounded)

paneIdOfJobType :: J.JobType -> PaneId
paneIdOfJobType = \case
  J.Wasp -> PaneWasp
  J.Server -> PaneServer
  J.WebApp -> PaneClient
  J.Db -> PaneDb

data PaneStatus
  = -- | Nothing has happened on this pane yet.
    StatusIdle
  | -- | The process is up but hasn't reported that it's serving yet.
    StatusStarting
  | -- | Work in progress: compiling, restarting, migrating, ...
    StatusWorking
  | -- | Up and serving.
    StatusReady
  | -- | Reported a failure it did not recover from.
    StatusFailed
  | StatusExited ExitCode
  deriving (Eq, Show)

data Pane = Pane
  { paneId :: !PaneId,
    paneLabel :: !T.Text,
    paneAccent :: !V.Color,
    paneBuffer :: !LogBuffer,
    -- | How many display rows we are scrolled up from the bottom. Zero means
    -- we're pinned to the latest output.
    paneScrollOffset :: !Int,
    -- | Whether new output should pull the view back to the bottom.
    paneIsFollowing :: !Bool,
    paneNumErrors :: !Int,
    paneNumWarnings :: !Int,
    -- | Output arrived while the user was looking at another tab.
    paneHasUnseen :: !Bool,
    paneStatus :: !PaneStatus
  }

makePane :: PaneId -> Pane
makePane pid =
  Pane
    { paneId = pid,
      paneLabel = label,
      paneAccent = accent,
      paneBuffer = emptyLogBuffer,
      paneScrollOffset = 0,
      paneIsFollowing = True,
      paneNumErrors = 0,
      paneNumWarnings = 0,
      paneHasUnseen = False,
      paneStatus = StatusIdle
    }
  where
    (label, accent) = case pid of
      -- The accents mirror the colors of the log prefixes in the non-TUI
      -- output, so the two modes feel like the same program.
      PaneWasp -> ("Wasp", Theme.brandYellow)
      PaneServer -> ("Server", Theme.brandPurple)
      PaneClient -> ("Client", Theme.colorCyan)
      PaneDb -> ("Db", Theme.colorBlue)

-- | Feeds a chunk of process output into the pane, updating its counters and
-- its status along the way. Also returns the lines the chunk completed, which
-- the caller needs to keep a paused pane looking at the same place.
feedPane :: Bool -> T.Text -> Pane -> (Pane, [LogLine])
feedPane isStderr chunk pane = (foldl' applyNewLine pane {paneBuffer = buffer'} newLines, newLines)
  where
    (buffer', newLines) = appendChunk isStderr chunk (paneBuffer pane)

    applyNewLine p line =
      p
        { paneNumErrors = paneNumErrors p + countOf LevelError line,
          paneNumWarnings = paneNumWarnings p + countOf LevelWarn line,
          paneHasUnseen = True,
          paneStatus = nextStatus (paneId p) line (paneStatus p)
        }

    countOf level line = if logLineLevel line == level then 1 else 0

-- | Status transitions driven by what a process says about itself.
--
-- We only react to unambiguous announcements: a stray "error" somewhere in a
-- stack trace shows up in the tab's error badge, but it doesn't flip a healthy
-- process to failed and leave it stuck there.
nextStatus :: PaneId -> LogLine -> PaneStatus -> PaneStatus
nextStatus pid line current = case pid of
  PaneServer
    | mentions ["[nodemon] app crashed"] -> StatusFailed
    | mentions ["server listening on"] -> StatusReady
    | mentions ["[nodemon] restarting", "[nodemon] starting"] -> StatusWorking
    | otherwise -> advanceFromIdle
  PaneClient
    | mentions ["ready in", "localhost:"] -> StatusReady
    | mentions ["page reload", "hmr update"] -> keepReady
    | otherwise -> advanceFromIdle
  PaneWasp
    | mentions ["failed to compile", "recompilation on file change failed"] -> StatusFailed
    | mentions ["recompiling"] -> StatusWorking
    | mentions ["watching your wasp project", "compiled", "recompilation on file change succeeded"] -> StatusReady
    | otherwise -> advanceFromIdle
  PaneDb
    | mentions ["database system is ready to accept connections"] -> StatusReady
    | otherwise -> advanceFromIdle
  where
    mentions = any (`T.isInfixOf` T.toLower (logLineText line))

    keepReady = if current == StatusReady then StatusReady else StatusWorking

    advanceFromIdle = case current of
      StatusIdle -> StatusStarting
      other -> other

markPaneExited :: ExitCode -> Pane -> Pane
markPaneExited exitCode pane = pane {paneStatus = StatusExited exitCode, paneHasUnseen = True}

clearPane :: Pane -> Pane
clearPane pane =
  pane
    { paneBuffer = clearLogBuffer (paneBuffer pane),
      paneScrollOffset = 0,
      paneIsFollowing = True,
      paneNumErrors = 0,
      paneNumWarnings = 0
    }

-- | Scrolls back by the given number of rows (negative scrolls forward, towards
-- the latest output).
--
-- How far back the pane can go depends on how its lines wrap, which depends on
-- how wide the pane is, so only the lower bound is enforced here: callers that
-- know the size clamp the result with 'setPaneScrollOffset'.
scrollPaneBy :: Int -> Pane -> Pane
scrollPaneBy rows pane = setPaneScrollOffset (paneScrollOffset pane + rows) pane

-- | Asks for the oldest line. The real offset is whatever the caller's clamp
-- turns this into; a line can wrap into any number of rows, so counting lines
-- would fall short.
scrollPaneToTop :: Pane -> Pane
scrollPaneToTop = setPaneScrollOffset maxScrollOffset

-- | Far more rows than a bounded buffer can ever wrap into, and small enough
-- that clamping arithmetic can't overflow.
maxScrollOffset :: Int
maxScrollOffset = maxBufferedLinesPerPane * maxRowsPerLine
  where
    maxRowsPerLine = 1000

setPaneScrollOffset :: Int -> Pane -> Pane
setPaneScrollOffset offset pane =
  pane
    { paneScrollOffset = clampedOffset,
      -- Scrolling back detaches from the latest output; scrolling all the way
      -- down reattaches, which is what every log viewer does.
      paneIsFollowing = clampedOffset == 0
    }
  where
    clampedOffset = max 0 (min maxScrollOffset offset)

scrollPaneToBottom :: Pane -> Pane
scrollPaneToBottom pane = pane {paneScrollOffset = 0, paneIsFollowing = True}

togglePaneFollow :: Pane -> Pane
togglePaneFollow pane =
  if paneIsFollowing pane
    then pane {paneIsFollowing = False}
    else scrollPaneToBottom pane

markPaneSeen :: Pane -> Pane
markPaneSeen pane = pane {paneHasUnseen = False}

-- | Not every tab is a server: the Wasp tab is a compiler and a file watcher,
-- and the Db tab is a series of one-shot Prisma commands. Their states read
-- better with their own words.
paneStatusLabel :: Pane -> T.Text
paneStatusLabel pane = case paneStatus pane of
  StatusIdle -> "idle"
  StatusStarting -> "starting"
  StatusWorking -> if isWasp then "compiling" else "working"
  StatusReady -> if isWasp then "watching" else "ready"
  StatusFailed -> "failed"
  -- A Prisma command finishing is the normal course of events, so saying it
  -- "exited" would make a healthy tab look like something went wrong.
  StatusExited ExitSuccess -> if isShortLived then "done" else "exited"
  StatusExited (ExitFailure code) -> "exited " <> T.pack (show code)
  where
    isWasp = paneId pane == PaneWasp
    isShortLived = paneId pane == PaneDb

-- | Whether the pane should show a spinner rather than a static dot.
paneIsBusy :: PaneStatus -> Bool
paneIsBusy status = status `elem` [StatusStarting, StatusWorking]
