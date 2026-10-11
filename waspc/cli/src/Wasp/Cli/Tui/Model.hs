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
    spinnerFrame,
    formatUptime,
  )
where

import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import qualified Wasp.Cli.Tui.Pane as Pane
import Wasp.Cli.Tui.Pane (Pane (..), PaneId (..))
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
    -- | Height in rows of the log body as of the last draw, used for paging.
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
    feed pid isStderr text = overPane pid (markSeenIfActive pid . Pane.feedPane isStderr text)

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
