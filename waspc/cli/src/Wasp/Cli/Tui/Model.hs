-- | State of the @wasp start@ TUI and the pure updates that drive it.
module Wasp.Cli.Tui.Model
  ( TuiName (..),
    TuiEvent (..),
    TuiState (..),
    makeTuiState,
    activePane,
    orderedPanes,
    withActivePane,
    applyTuiEvent,
    selectPane,
    selectPaneByIndex,
    selectNextPane,
    selectPreviousPane,
    isAnyPaneBusy,
    scrollActivePaneBy,
    scrollActivePaneToTop,
    scrollActivePaneToBottom,
    toggleActivePaneFollow,
    setTerminalSize,
    logBodyHeightFor,
    spinnerFrame,
    formatUptime,
  )
where

import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Wasp.Cli.Tui.Layout (logContentWidthFor, visibleLogRows, wrapLogLine)
import Wasp.Cli.Tui.Pane (Pane (..), PaneId (..))
import qualified Wasp.Cli.Tui.Pane as Pane
import qualified Wasp.Job as J

-- | Resource names. The log body is drawn by hand rather than with a brick
-- viewport (we need to know the exact wrapped-row count to scroll by), so
-- there is only one name and it exists to satisfy brick's type parameter.
data TuiName = LogBodyName
  deriving (Eq, Ord, Show)

data TuiEvent
  = -- | Output or exit of one of the generated app's processes.
    TuiJobMessage J.JobMessage
  | -- | A message from Wasp itself (compiler, file watcher).
    TuiWaspOutput T.Text
  | -- | Spinner / uptime tick, carrying the time it fired at.
    TuiTick UTCTime

data TuiState = TuiState
  { tuiPanes :: !(M.Map PaneId Pane),
    tuiActivePaneId :: !PaneId,
    tuiProjectName :: !T.Text,
    -- | Label and URL of each place the app is reachable, shown in the header.
    tuiAppUrls :: ![(T.Text, T.Text)],
    tuiTickCount :: !Int,
    tuiStartedAt :: !UTCTime,
    tuiNow :: !UTCTime,
    tuiShowHelp :: !Bool,
    -- | Set by the first @q@, cleared by any other key. Stopping a dev server
    -- by accident is expensive enough to be worth a second keystroke.
    tuiIsQuitArmed :: !Bool,
    -- | Size of the log body as of the last known terminal size. Scrolling
    -- needs it: how far back a pane can go depends on how its lines wrap.
    tuiLogBodyWidth :: !Int,
    tuiLogBodyHeight :: !Int
  }

-- | The panes we always show. The Db pane is included even though plain
-- @wasp start@ doesn't run a database process: it is where database output
-- lands when something (a migration, a seed) produces any.
paneOrder :: [PaneId]
paneOrder = [PaneWasp, PaneServer, PaneClient, PaneDb]

makeTuiState :: T.Text -> [(T.Text, T.Text)] -> UTCTime -> TuiState
makeTuiState projectName appUrls startedAt =
  TuiState
    { tuiPanes = M.fromList [(pid, Pane.makePane pid) | pid <- paneOrder],
      tuiActivePaneId = PaneServer,
      tuiProjectName = projectName,
      tuiAppUrls = appUrls,
      tuiTickCount = 0,
      tuiStartedAt = startedAt,
      tuiNow = startedAt,
      tuiShowHelp = False,
      tuiIsQuitArmed = False,
      tuiLogBodyWidth = 80,
      tuiLogBodyHeight = 20
    }

orderedPanes :: TuiState -> [Pane]
orderedPanes state = [pane | pid <- paneOrder, Just pane <- [M.lookup pid (tuiPanes state)]]

activePane :: TuiState -> Pane
activePane state =
  fromMaybe (Pane.makePane (tuiActivePaneId state)) $
    M.lookup (tuiActivePaneId state) (tuiPanes state)

withActivePane :: (Pane -> Pane) -> TuiState -> TuiState
withActivePane f state = state {tuiPanes = M.adjust f (tuiActivePaneId state) (tuiPanes state)}

applyTuiEvent :: TuiEvent -> TuiState -> TuiState
applyTuiEvent event state = case event of
  TuiTick now -> state {tuiTickCount = tuiTickCount state + 1, tuiNow = now}
  TuiWaspOutput text -> feed PaneWasp False text state
  TuiJobMessage jobMessage ->
    let pid = Pane.paneIdOfJobType (J._jobType jobMessage)
     in case J._data jobMessage of
          J.JobOutput text outputType -> feed pid (outputType == J.Stderr) text state
          J.JobExit exitCode -> overPane pid (Pane.markPaneExited exitCode) state
  where
    feed pid isStderr text = overPane pid (markSeenIfActive pid . appendOutput isStderr text)

    -- A pane the user has scrolled back in should keep showing what it was
    -- showing, so new output pushes its scroll position along instead of
    -- sliding the text out from under them.
    appendOutput isStderr text pane =
      let (pane', newLines) = Pane.feedPane isStderr text pane
       in if paneIsFollowing pane'
            then pane'
            else
              Pane.scrollPaneBy
                (sum [length (wrapLogLine logContentWidth line) | line <- newLines])
                pane'

    logContentWidth = logContentWidthFor (tuiLogBodyWidth state)

    -- Output that lands on the tab the user is already looking at isn't
    -- "unseen", so the tab bar shouldn't nag about it.
    markSeenIfActive pid pane =
      if pid == tuiActivePaneId state then Pane.markPaneSeen pane else pane

overPane :: PaneId -> (Pane -> Pane) -> TuiState -> TuiState
overPane pid f state = state {tuiPanes = M.adjust f pid (tuiPanes state)}

selectPane :: PaneId -> TuiState -> TuiState
selectPane pid state =
  (overPane pid Pane.markPaneSeen state) {tuiActivePaneId = pid}

selectPaneByIndex :: Int -> TuiState -> TuiState
selectPaneByIndex index state = case drop index paneOrder of
  (pid : _) | index >= 0 -> selectPane pid state
  _ -> state

selectNextPane :: TuiState -> TuiState
selectNextPane = selectRelativePane 1

selectPreviousPane :: TuiState -> TuiState
selectPreviousPane = selectRelativePane (-1)

selectRelativePane :: Int -> TuiState -> TuiState
selectRelativePane step state = selectPane nextPaneId state
  where
    currentIndex = fromMaybe 0 $ elemIndex (tuiActivePaneId state) paneOrder
    nextPaneId = paneOrder !! ((currentIndex + step) `mod` length paneOrder)

-- | Whether any pane is doing something that the UI animates.
isAnyPaneBusy :: TuiState -> Bool
isAnyPaneBusy = any (Pane.paneIsBusy . paneStatus) . orderedPanes

-- | Scrolls the active pane back by the given number of display rows
-- (negative scrolls forward), stopping at the oldest line.
scrollActivePaneBy :: Int -> TuiState -> TuiState
scrollActivePaneBy rows = clampActivePaneScroll . withActivePane (Pane.scrollPaneBy rows)

scrollActivePaneToTop :: TuiState -> TuiState
scrollActivePaneToTop = clampActivePaneScroll . withActivePane Pane.scrollPaneToTop

scrollActivePaneToBottom :: TuiState -> TuiState
scrollActivePaneToBottom = withActivePane Pane.scrollPaneToBottom

toggleActivePaneFollow :: TuiState -> TuiState
toggleActivePaneFollow = clampActivePaneScroll . withActivePane Pane.togglePaneFollow

-- | Brings the scroll position down to what the pane actually has to show at
-- the current size. A pane can only answer that once it knows how wide it is,
-- since a long line wraps into several rows.
clampActivePaneScroll :: TuiState -> TuiState
clampActivePaneScroll state = withActivePane clamp state
  where
    clamp pane
      | paneIsFollowing pane = pane
      | otherwise = Pane.setPaneScrollOffset clampedOffset pane
      where
        (_, clampedOffset) =
          visibleLogRows (logContentWidthFor (tuiLogBodyWidth state)) (tuiLogBodyHeight state) pane

setTerminalSize :: Int -> Int -> TuiState -> TuiState
setTerminalSize width height state =
  clampActivePaneScroll
    state
      { tuiLogBodyWidth = width,
        tuiLogBodyHeight = logBodyHeightFor height
      }

-- | Rows of chrome above and below the log body.
numChromeRowsAbove, numChromeRowsBelow :: Int
numChromeRowsAbove = 5
numChromeRowsBelow = 2

-- | How tall the log body is in a terminal of the given height.
logBodyHeightFor :: Int -> Int
logBodyHeightFor terminalHeight = max 1 (terminalHeight - numChromeRowsAbove - numChromeRowsBelow)

-- | Braille spinner, the same one the rest of the ecosystem uses.
spinnerFrame :: Int -> T.Text
spinnerFrame tick = T.singleton (frames !! (tick `mod` length frames))
  where
    frames = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

-- | How long the TUI has been up, as a short human-readable string.
formatUptime :: TuiState -> T.Text
formatUptime state
  | hours > 0 = tshow hours <> "h " <> pad minutes <> "m " <> pad seconds <> "s"
  | minutes > 0 = tshow minutes <> "m " <> pad seconds <> "s"
  | otherwise = tshow seconds <> "s"
  where
    totalSeconds = max 0 $ floor (tuiNow state `diffUTCTime` tuiStartedAt state) :: Int
    hours = totalSeconds `div` 3600
    minutes = (totalSeconds `mod` 3600) `div` 60
    seconds = totalSeconds `mod` 60
    tshow = T.pack . show
    pad n = if n < 10 then "0" <> tshow n else tshow n
