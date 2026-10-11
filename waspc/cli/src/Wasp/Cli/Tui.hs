-- | A full-screen terminal UI for @wasp start@.
--
-- @wasp start@ runs several processes at once (the server, the client, and
-- Wasp's own compiler watching for changes) and, without a UI, their output is
-- interleaved into one scrolling stream. This module gives each of them its
-- own tab with its own scrollback, and surfaces at a glance what is running,
-- what is ready, and what has errors.
--
-- The TUI owns the terminal while it runs, so everything that would otherwise
-- be printed to stdout has to be routed into it instead: callers get a
-- 'TuiHandle' with a channel for process output and a 'Msg.SendMessage' for
-- Wasp's own messages.
module Wasp.Cli.Tui
  ( TuiConfig (..),
    TuiHandle (..),
    withTui,
    isTuiSupported,
  )
where

import Brick (App (..), BrickEvent (..), EventM, Widget, halt, neverShowCursor)
import qualified Brick.BChan as BChan
import qualified Brick.Main as BrickMain
import Control.Concurrent (Chan, newChan, readChan, threadDelay)
import Control.Concurrent.Async (race, withAsync)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, modify)
import Data.Char (digitToInt, isDigit)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdin, stdout)
import Wasp.Cli.Tui.Draw (drawTui)
import Wasp.Cli.Tui.Model
  ( TuiEvent (..),
    TuiName,
    TuiState (..),
    applyTuiEvent,
    makeTuiState,
    scrollActivePaneBy,
    scrollActivePaneToBottom,
    scrollActivePaneToTop,
    selectNextPane,
    selectPaneByIndex,
    selectPreviousPane,
    setTerminalSize,
    toggleActivePaneFollow,
    withActivePane,
  )
import qualified Wasp.Cli.Tui.Pane as Pane
import qualified Wasp.Cli.Tui.Theme as Theme
import qualified Wasp.Job as J
import qualified Wasp.Message as Msg

data TuiConfig = TuiConfig
  { tuiConfigProjectName :: T.Text,
    -- | Label and URL of each place the running app is reachable.
    tuiConfigAppUrls :: [(T.Text, T.Text)]
  }

-- | The ways to get output into a running TUI.
data TuiHandle = TuiHandle
  { -- | Output of the generated app's processes, routed to a tab by job type.
    tuiJobMessageChan :: Chan J.JobMessage,
    -- | Wasp's own messages (compilation, file watching), shown in the Wasp tab.
    tuiSendWaspMessage :: Msg.SendMessage
  }

-- | Whether this terminal can host the TUI. A pipe, a dumb terminal, or a CI
-- job gets the plain scrolling output instead.
isTuiSupported :: IO Bool
isTuiSupported = do
  stdoutIsTerminal <- hIsTerminalDevice stdout
  stdinIsTerminal <- hIsTerminalDevice stdin
  term <- lookupEnv "TERM"
  isCi <- isJust <$> lookupEnv "CI"
  return $
    and
      [ stdoutIsTerminal,
        stdinIsTerminal,
        maybe False (`notElem` ["", "dumb"]) term,
        not isCi
      ]

-- | Runs the TUI alongside the given action, which receives the handle it
-- should send all of its output to.
--
-- Returns 'Nothing' when the user quit the TUI (the action is then cancelled,
-- which stops the processes it started), and the action's result otherwise.
withTui :: TuiConfig -> (TuiHandle -> IO a) -> IO (Maybe a)
withTui config action = do
  jobMessageChan <- newChan
  eventChan <- BChan.newBChan eventChanCapacity
  startTime <- getCurrentTime

  let handle =
        TuiHandle
          { tuiJobMessageChan = jobMessageChan,
            tuiSendWaspMessage = sendWaspMessageToTui eventChan
          }
      initialState =
        makeTuiState (tuiConfigProjectName config) (tuiConfigAppUrls config) startTime

  withAsync (forwardJobMessages jobMessageChan eventChan) $ \_ ->
    withAsync (sendTicks eventChan) $ \_ -> do
      outcome <- runTuiApp eventChan initialState `race` action handle
      return $ either (const Nothing) Just outcome
  where
    -- Big enough that a burst of output from a dev server doesn't block the
    -- process reading it, small enough to keep backpressure if the UI stalls.
    eventChanCapacity = 2048

runTuiApp :: BChan.BChan TuiEvent -> TuiState -> IO ()
runTuiApp eventChan initialState = do
  let buildVty = mkVty V.defaultConfig
  initialVty <- buildVty
  void $ BrickMain.customMain initialVty buildVty (Just eventChan) tuiApp initialState

forwardJobMessages :: Chan J.JobMessage -> BChan.BChan TuiEvent -> IO ()
forwardJobMessages jobMessageChan eventChan =
  forever $ readChan jobMessageChan >>= BChan.writeBChan eventChan . TuiJobMessage

-- | Drives the spinners and the uptime counter.
sendTicks :: BChan.BChan TuiEvent -> IO ()
sendTicks eventChan = forever $ do
  threadDelay (120 * 1000)
  now <- getCurrentTime
  BChan.writeBChan eventChan (TuiTick now)

-- | Renders a Wasp message into the Wasp tab.
--
-- The message is colored with ANSI escape codes rather than with Vty
-- attributes so that it travels the same path as process output, which is
-- already full of them.
sendWaspMessageToTui :: BChan.BChan TuiEvent -> Msg.Message -> IO ()
sendWaspMessageToTui eventChan message =
  BChan.writeBChan eventChan $ TuiWaspOutput (render message <> "\n")
  where
    render = \case
      Msg.Info body -> T.pack body
      Msg.Start body -> colored yellow $ "▸ " <> T.pack body
      Msg.Success body -> colored green $ "✔ " <> T.pack body
      Msg.Failure title body -> colored red $ "✖ " <> T.pack title <> ":\n" <> T.pack body
      Msg.Warning title body -> colored amber $ "! " <> T.pack title <> ":\n" <> T.pack body

    colored colorCode body = "\ESC[" <> colorCode <> "m" <> body <> "\ESC[0m"
    (yellow, green, red, amber) = ("33", "32", "31", "33")

tuiApp :: App TuiState TuiEvent TuiName
tuiApp =
  App
    { appDraw = drawTui :: TuiState -> [Widget TuiName],
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleTuiEvent,
      appStartEvent = syncTerminalSize,
      appAttrMap = const Theme.tuiAttrMap
    }

-- | Tells the model how big the terminal is, which it needs to scroll by a
-- screenful and to know how far back a pane's wrapped lines reach.
syncTerminalSize :: EventM TuiName TuiState ()
syncTerminalSize = do
  vty <- BrickMain.getVtyHandle
  (terminalWidth, terminalHeight) <- liftIO $ V.displayBounds (V.outputIface vty)
  modify $ setTerminalSize terminalWidth terminalHeight

handleTuiEvent :: BrickEvent TuiName TuiEvent -> EventM TuiName TuiState ()
handleTuiEvent = \case
  AppEvent event -> modify (applyTuiEvent event)
  VtyEvent (V.EvResize terminalWidth terminalHeight) ->
    modify $ setTerminalSize terminalWidth terminalHeight
  VtyEvent (V.EvKey key modifiers) -> do
    state <- get
    -- A pending quit confirmation is cleared by whatever key comes next, so
    -- that only a deliberate double tap of @q@ stops the app.
    modify $ \s -> s {tuiIsQuitArmed = False}
    if tuiIsQuitArmed state && (key, modifiers) == (V.KChar 'q', [])
      then halt
      else handleKeyPress key modifiers
  _ -> return ()

handleKeyPress :: V.Key -> [V.Modifier] -> EventM TuiName TuiState ()
handleKeyPress key modifiers = case (key, modifiers) of
  -- Ctrl-C is unambiguous, so it doesn't ask for confirmation.
  (V.KChar 'c', [V.MCtrl]) -> halt
  (V.KChar 'q', []) -> modify $ \state -> state {tuiIsQuitArmed = True, tuiShowHelp = False}
  (V.KChar '?', []) -> modify toggleHelp
  (V.KEsc, []) -> modify (\state -> state {tuiShowHelp = False})
  -- Switching tabs.
  (V.KChar '\t', []) -> modify selectNextPane
  (V.KBackTab, []) -> modify selectPreviousPane
  (V.KRight, []) -> modify selectNextPane
  (V.KLeft, []) -> modify selectPreviousPane
  (V.KChar 'l', []) -> modify selectNextPane
  (V.KChar 'h', []) -> modify selectPreviousPane
  (V.KChar digit, []) | isDigit digit && digit /= '0' -> modify (selectPaneByIndex (digitToInt digit - 1))
  -- Scrolling. Scrolling "up" means going back in history.
  (V.KUp, []) -> scrollBy 1
  (V.KChar 'k', []) -> scrollBy 1
  (V.KDown, []) -> scrollBy (-1)
  (V.KChar 'j', []) -> scrollBy (-1)
  (V.KPageUp, []) -> scrollByPage 1
  (V.KPageDown, []) -> scrollByPage (-1)
  (V.KChar 'b', [V.MCtrl]) -> scrollByPage 1
  (V.KChar 'f', [V.MCtrl]) -> scrollByPage (-1)
  (V.KChar 'u', [V.MCtrl]) -> scrollByHalfPage 1
  (V.KChar 'd', [V.MCtrl]) -> scrollByHalfPage (-1)
  (V.KChar 'g', []) -> modify scrollActivePaneToTop
  (V.KHome, []) -> modify scrollActivePaneToTop
  (V.KChar 'G', []) -> modify scrollActivePaneToBottom
  (V.KEnd, []) -> modify scrollActivePaneToBottom
  (V.KChar 'f', []) -> modify toggleActivePaneFollow
  (V.KChar 'c', []) -> modify (withActivePane Pane.clearPane)
  _ -> return ()
  where
    toggleHelp state = state {tuiShowHelp = not (tuiShowHelp state)}

scrollBy :: Int -> EventM TuiName TuiState ()
scrollBy rows = modify (scrollActivePaneBy rows)

-- | A page keeps one row of context from the previous screenful.
scrollByPage :: Int -> EventM TuiName TuiState ()
scrollByPage pages = do
  state <- get
  scrollBy (pages * max 1 (tuiLogBodyHeight state - 1))

scrollByHalfPage :: Int -> EventM TuiName TuiState ()
scrollByHalfPage halfPages = do
  state <- get
  scrollBy (halfPages * max 1 (tuiLogBodyHeight state `div` 2))
