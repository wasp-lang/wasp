module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Environment (lookupEnv)
import Wasp.AppComponentUrl (AppComponentUrl)
import qualified Wasp.AppComponentUrl as AppComponentUrl
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentPorts (findAppComponentPorts)
import Wasp.Cli.AppComponentUrls (makeAppComponentUrls)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (compile, defaultCompileOptions, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.News (fetchAndListMustSeeNewsIfDue)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Start.ArgumentsParser (StartArgs (..), startArgsParser)
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.EnvVarWithCtx (addEnvVarsUniqueC)
import qualified Wasp.Cli.EnvVarWithCtx as EnvVarWithCtx
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Cli.RunConfigs (makeRunConfigs, showRunConfigUrls)
import qualified Wasp.Cli.Tui as Tui
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig (..))
import qualified Wasp.Job as J
import Wasp.Job.Process (StdinMode (..))
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)
import Wasp.Project.Common (WaspProjectDir, findFileInWaspProjectDir, generatedAppDirInWaspProjectDir)
import qualified Wasp.Project.Env as Env

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Arguments -> Command ()
start = withArguments startArgsParser $ \args -> withProjectLock $ do
  -- We check for the news only in `wasp start`, and only periodically,
  -- to avoid being too aggressive. Specifically:
  --   - We don't run it in other `wasp` commands because we don't want to
  --     accidentally trigger news in CI (and `wasp start` is rarely used in
  --     normal CI, except for maybe e2e testing).
  --   - It would be annoying if news came out at you while you were doing
  --     something like `wasp db migrate-dev`.
  -- Therefore, it's best to keep the periodic news check contained and
  -- expected. This way we know exactly which workflows it could possibly
  -- interrupt (LLMs, CIs, people...).
  liftIO fetchAndListMustSeeNewsIfDue
  InWaspProject waspProjectDir <- require
  let outDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  (warnings, appSpec) <- compile

  appComponentUrls <- makeDevAppComponentUrls appSpec args
  let runConfigs = makeRunConfigs appComponentUrls
  assertImplicitEnvVarsDontOverrideWaspEnvVars waspProjectDir runConfigs

  DbConnectionEstablished <- require

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."
  cliSendMessageC $ Msg.Info $ showRunConfigUrls runConfigs

  useTui <- liftIO $ shouldUseTui args

  -- Everything below runs until the user stops it: the only way out is an
  -- error from one of the processes, or the user quitting the TUI.
  let runProjectWith compileOptions startOptions = do
        let sendMessage = compileOptions.sendMessage
        -- This MVar is used to exchange information between the two processes below running in
        -- parallel, specifically to allow us to pass the results of re-compilation done by 'watch'
        -- into the 'onJobsQuietDown' handler used by 'startWebApp'.
        -- This way we can show newest Wasp compile warnings and errors (produced by recompilation from
        -- 'watch') once jobs from 'start' quiet down a bit.
        ongoingCompilationResultMVar <- newMVar (warnings, [])
        let watchWaspProjectSource =
              watch compileOptions outDir ongoingCompilationResultMVar
        let startGeneratedWebApp =
              Wasp.Generator.start
                startOptions
                runConfigs
                waspProjectDir
                outDir
                (onJobsQuietDown sendMessage ongoingCompilationResultMVar)
        -- In parallel:
        -- 1. watch for any changes in the Wasp project, be it users wasp code or users JS/HTML/...
        --    code. On any change, Wasp is recompiled (and generated app is re-generated).
        -- 2. start web app in dev mode, which will then also watch for changes but in the generated
        --    code, and will also react to them by restarting the web app.
        -- Both of these should run forever, unless some super serious error happens.
        watchWaspProjectSource `race` startGeneratedWebApp

  maybeWatchOrStartResult <-
    liftIO $
      if useTui
        then Tui.withTui (makeTuiConfig waspProjectDir runConfigs) $ \tuiHandle -> do
          -- The Wasp tab would otherwise start out empty, since compilation
          -- already happened above, before the TUI took over the terminal.
          reportInitialCompilationToTui (Tui.tuiSendWaspMessage tuiHandle) warnings
          runProjectWith
            (defaultCompileOptions waspProjectDir)
              { sendMessage = Tui.tuiSendWaspMessage tuiHandle,
                consumeJobMessages = forwardJobMessagesUntilExit (Tui.tuiJobMessageChan tuiHandle)
              }
            Wasp.Generator.StartOptions
              { Wasp.Generator.stdinMode = ClosedStdin,
                Wasp.Generator.consumeJobMessages =
                  forwardJobMessagesUntilExit (Tui.tuiJobMessageChan tuiHandle)
              }
        else
          Just
            <$> runProjectWith
              (defaultCompileOptions waspProjectDir)
              Wasp.Generator.defaultStartOptions

  case maybeWatchOrStartResult of
    -- The user quit the TUI, which also stopped everything it was running.
    Nothing -> cliSendMessageC $ Msg.Success "Stopped. Run `wasp start` to pick up where you left off."
    Just (Left ()) -> error "This should never happen, listening for file changes should never end but it did."
    Just (Right startResult) -> case startResult of
      Left startError -> throwError $ CommandError "Start failed" startError
      Right () -> error "This should never happen, start should never end but it did."
  where
    onJobsQuietDown :: Msg.SendMessage -> MVar ([CompileWarning], [CompileError]) -> IO ()
    onJobsQuietDown sendMessage ongoingCompilationResultMVar = do
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
          sendMessage $ Msg.Info ""
          printWarningsAndErrorsIfAny sendMessage (warnings, errors)
          sendMessage $ Msg.Info ""

-- | Replays the result of the compilation that `wasp start` did before the TUI
-- opened, so that the Wasp tab says where things stand.
reportInitialCompilationToTui :: Msg.SendMessage -> [CompileWarning] -> IO ()
reportInitialCompilationToTui sendMessage warnings = do
  sendMessage $ Msg.Success "Compiled your Wasp project."
  printWarningsAndErrorsIfAny sendMessage (warnings, [])
  sendMessage $ Msg.Start "Watching your Wasp project for changes..."

-- | Hands every job message over to the TUI, and stops once a process has
-- exited (mirroring what the plain log printer does, so that 'Wasp.Generator.start'
-- can tell that the run is over).
forwardJobMessagesUntilExit :: Chan J.JobMessage -> Chan J.JobMessage -> IO ()
forwardJobMessagesUntilExit tuiChan jobsChan = do
  jobMessage <- readChan jobsChan
  writeChan tuiChan jobMessage
  case J._data jobMessage of
    J.JobExit {} -> return ()
    J.JobOutput {} -> forwardJobMessagesUntilExit tuiChan jobsChan

-- | The TUI is the default, but it needs a real terminal, and there are plenty
-- of reasons to want the plain logs instead: piping them to a file, debugging
-- Wasp itself, or simply preferring them.
shouldUseTui :: StartArgs -> IO Bool
shouldUseTui args = do
  disabledByEnvVar <- (== Just "0") <$> lookupEnv "WASP_TUI"
  if args.noTui || disabledByEnvVar
    then return False
    else Tui.isTuiSupported

makeTuiConfig :: Path' Abs (Dir WaspProjectDir) -> (WebAppRunConfig, ServerRunConfig) -> Tui.TuiConfig
makeTuiConfig waspProjectDir (clientRunConfig, serverRunConfig) =
  Tui.TuiConfig
    { -- 'fromRelDir' leaves a trailing separator, which we don't want in a name.
      Tui.tuiConfigProjectName =
        T.dropWhileEnd (`elem` ("/\\" :: String)) $ T.pack $ SP.fromRelDir $ SP.basename waspProjectDir,
      Tui.tuiConfigAppUrls =
        [ ("client", withTrailingSlash clientRunConfig.url),
          ("server", withTrailingSlash serverRunConfig.url)
        ]
    }
  where
    -- The client and server URLs disagree about trailing slashes, so we settle
    -- it for display, the same way `showRunConfigUrls` does.
    withTrailingSlash appComponentUrl =
      let url = T.pack $ AppComponentUrl.url appComponentUrl
       in if "/" `T.isSuffixOf` url then url else url <> "/"

makeDevAppComponentUrls :: AppSpec -> StartArgs -> Command (AppComponentUrl, AppComponentUrl)
makeDevAppComponentUrls appSpec args = do
  ports <- findAppComponentPorts (args.clientPort, args.serverPort)
  return $ makeAppComponentUrls appSpec ports (args.clientUrl, args.serverUrl)

-- | The web app and server have their own logic for reading environment
-- variables autonomously, so we don't need to merge the different sources of
-- environment variables ourselves
-- (https://github.com/wasp-lang/wasp/issues/4739). However, we should still
-- check that they do not conflict with the environment variables that Wasp
-- itself uses to tell these apps where to run.
assertImplicitEnvVarsDontOverrideWaspEnvVars :: Path' Abs (Dir WaspProjectDir) -> (WebAppRunConfig, ServerRunConfig) -> Command ()
assertImplicitEnvVarsDontOverrideWaspEnvVars waspProjectDir (clientRunConfig, serverRunConfig) = do
  implicitClientEnvVars <- liftIO $ readImplicitEnvVars Env.dotEnvClient
  implicitServerEnvVars <- liftIO $ readImplicitEnvVars Env.dotEnvServer

  -- We only use this to check for env vars being overriden. We throw away the
  -- merged env vars, because the generated apps will read the .env files and
  -- inherited environment themselves.
  _ <- implicitClientEnvVars `addEnvVarsUniqueC` clientRunConfig
  _ <- implicitServerEnvVars `addEnvVarsUniqueC` serverRunConfig

  return ()
  where
    readImplicitEnvVars dotEnvFile =
      mconcat
        [ readProjectFileIfExists dotEnvFile,
          EnvVarWithCtx.readEnvironment
        ]

    readProjectFileIfExists dotEnvFile =
      findFileInWaspProjectDir waspProjectDir dotEnvFile
        >>= maybe (return []) (EnvVarWithCtx.readDotEnvFile (SP.fromRelFile dotEnvFile))
