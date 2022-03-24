module Wasp.Cli.Command.Start
  ( start,
  )
where

import Brick
  ( App (..),
    AttrMap,
    BrickEvent (AppEvent, VtyEvent),
    EventM,
    Next,
    Widget,
    attrMap,
    continue,
    customMain,
    halt,
    neverShowCursor,
    txt,
  )
import Brick.BChan (newBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, race)
import qualified Control.Concurrent.Async as Async
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..), runCommand)
import Wasp.Cli.Command.Common
  ( findWaspProjectRootDirFromCwd,
  )
import Wasp.Cli.Command.Compile
  ( compileIO,
  )
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Lib
import qualified Wasp.Message as Msg

type Name = ()

type State = T.Text

data Msg = Msg T.Text

app :: App State Msg Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const theMap
    }

drawUI :: State -> [Widget Name]
drawUI = return . txt

handleEvent :: State -> BrickEvent Name Msg -> EventM Name (Next State)
handleEvent s (AppEvent (Msg t)) = continue (s <> "\n" <> t)
handleEvent s (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt s
handleEvent s (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent s (VtyEvent _) = continue s
handleEvent s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr []

start :: IO ()
start = do
  chan <- newBChan 10

  threadId <- Async.async $ do
    delay 1 -- hack to let customMain start before first messages sent to stdout
    runCommand startCommand

  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _ <- customMain initialVty buildVty (Just chan) app "Wasp Start"

  -- TODO: figure out why it leaves behind stale react-scripts server process (but nodemon exits)
  cancel threadId
  where
    delay sec = threadDelay (sec * 1000000)

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
startCommand :: Command ()
startCommand = do
  waspRoot <- findWaspProjectRootDirFromCwd
  let outDir = waspRoot </> Common.dotWaspDirInWaspProjectDir </> Common.generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."
  compilationResult <- liftIO $ compileIO waspRoot outDir
  case compilationResult of
    Left compileError -> throwError $ CommandError "Compilation failed" compileError
    Right () -> cliSendMessageC $ Msg.Success "Code has been successfully compiled, project has been generated."

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."
  watchOrStartResult <- liftIO $ race (watch waspRoot outDir) (Wasp.Lib.start outDir)
  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."
