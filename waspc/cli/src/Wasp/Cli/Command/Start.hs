module Wasp.Cli.Command.Start
  ( start,
  )
where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as B
import Control.Concurrent ()
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty
import qualified Graphics.Vty as Vty
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compile, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (DbConnectionEstablished (DbConnectionEstablished), FromOutDir (FromOutDir), InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Watch (watch)
import qualified Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)

-- TODO:
-- This is just some initial experimentation, but it looks very promising. Very little, simple code, to get what we need.
--
-- Main thing that will be missing is escaping the ansii control characters. Brick and Vty can't work with them,
-- so to show the output from underlying processes, we will need to escape/rewrite them -> replace them with Vty's styling.
-- This shouldn't be so hard, especially with AI's help -> we can handle most common ones and just skip the rest.
-- Handling of \r will also be interesting, sometihng we can consider.
--
-- Then, we can just pipe all the output from the processes into the Brick's BChan, if I am correct,
-- and re-render whenever a new info comes.
--
-- We will want to wrap logs into vertical scroll and then maybe do some optimizing regarding what is drawn (maybe not).
--
-- There is even support for animations, if we want to play with that.
--
-- Btw here is the central piece of Brick docs: https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst .

start :: Command ()
start = liftIO $ do
  B.defaultMain app ()

-- | Resource name
data ResName = NoNamesYet
  deriving (Ord, Eq)

app :: B.App () e ResName
app =
  B.App
    { appDraw = const [ui],
      appHandleEvent = \case
        B.VtyEvent (Vty.EvKey (Vty.KChar 'q') []) -> B.halt
        _otherEvent -> pure (),
      appStartEvent = pure (),
      appAttrMap =
        const $
          B.attrMap
            Vty.defAttr
            [ (B.attrName "waspLogo", B.fg Vty.yellow `Vty.withStyle` Vty.bold),
              (B.attrName "selectedTab", Vty.black `B.on` Vty.yellow `Vty.withStyle` Vty.bold)
            ],
      appChooseCursor = B.neverShowCursor
    }

ui :: B.Widget ResName
ui =
  B.joinBorders $
    B.withBorderStyle B.unicode $
      B.vBox
        [ B.border $
            B.vLimit 1 $
              B.hBox
                [ B.withAttr (B.attrName "waspLogo") $ B.padLeftRight 1 $ B.str "Wasp =}",
                  B.vBorder,
                  B.fill ' ',
                  B.padLeftRight 1 $ B.str "1:Client",
                  B.withAttr (B.attrName "selectedTab") $ B.padLeftRight 1 $ B.str "2:Server",
                  B.padLeftRight 1 $ B.str "3:Db",
                  B.padLeftRight 1 $ B.str "4:All"
                ],
          B.padAll 1 $ B.center $ B.str "Here come the logs!",
          B.border $ B.vLimit 1 $ B.hBox [B.padLeftRight 1 $ B.str "footer", B.fill ' ']
        ]

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start' :: Command ()
start' = do
  InWaspProject waspProjectDir <- require
  let outDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  warnings <- compile

  DbConnectionEstablished FromOutDir <- require

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."

  watchOrStartResult <- liftIO $ do
    -- This MVar is used to exchange information between the two processes below running in
    -- parallel, specifically to allow us to pass the results of re-compilation done by 'watch'
    -- into the 'onJobsQuietDown' handler used by 'startWebApp'.
    -- This way we can show newest Wasp compile warnings and errors (produced by recompilation from
    -- 'watch') once jobs from 'start' quiet down a bit.
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspProjectDir outDir ongoingCompilationResultMVar
    let startGeneratedWebApp = Wasp.Generator.start outDir (onJobsQuietDown ongoingCompilationResultMVar)
    -- In parallel:
    -- 1. watch for any changes in the Wasp project, be it users wasp code or users JS/HTML/...
    --    code. On any change, Wasp is recompiled (and generated code is re-generated).
    -- 2. start web app in dev mode, which will then also watch for changes but in the generated
    --    code, and will also react to them by restarting the web app.
    -- Both of these should run forever, unless some super serious error happens.
    watchWaspProjectSource `race` startGeneratedWebApp

  case watchOrStartResult of
    Left () -> error "This should never happen, listening for file changes should never end but it did."
    Right startResult -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."
  where
    onJobsQuietDown :: MVar ([CompileWarning], [CompileError]) -> IO ()
    onJobsQuietDown ongoingCompilationResultMVar = do
      -- Once jobs from generated web app quiet down a bit, we print any warnings / errors from the
      -- latest (re)compile that haven't yet been printed in this situation.
      -- This way we ensure that even if web app jobs print a lot of output, users
      -- won't miss wasp compiler warnings and errors, since they will be again printed after all
      -- of that output.
      maybeOngoingCompilationResult <- tryTakeMVar ongoingCompilationResultMVar
      case maybeOngoingCompilationResult of
        Nothing -> return ()
        Just ([], []) -> return ()
        Just (warnings, errors) -> do
          putStrLn ""
          printWarningsAndErrorsIfAny (warnings, errors)
          putStrLn ""
