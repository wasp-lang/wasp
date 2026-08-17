module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.AppComponentUrl (AppComponentUrl (..))
import Wasp.AppSpec (AppSpec)
import Wasp.Cli.AppComponentPorts (findAppComponentPorts)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (compile, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.News (fetchAndListMustSeeNewsIfDue)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Start.ArgumentsParser (StartArgs (..), startArgsParser)
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.EnvVarCtx (addEnvVarsUniqueC)
import qualified Wasp.Cli.EnvVarCtx as EnvVarCtx
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Cli.RunConfigs (defaultDevServerUrl, makeDefaultDevClientUrl, makeRunConfigs, showAppComponentUrls)
import Wasp.Cli.Util.Parser (withArguments)
import qualified Wasp.Generator
import Wasp.Generator.ServerGenerator.RunConfig (ServerRunConfig (..))
import Wasp.Generator.WebAppGenerator.RunConfig (WebAppRunConfig)
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import qualified Wasp.Project.Env as Env

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Arguments -> Command ()
start = withArguments "wasp start" startArgsParser $ \args -> withProjectLock $ do
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

  appComponentUrls <- makeDevAppUrls appSpec args
  runConfigs <- makeDevRunConfigs waspProjectDir appComponentUrls

  DbConnectionEstablished <- require

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."
  cliSendMessageC $ Msg.Info $ showAppComponentUrls appComponentUrls

  watchOrStartResult <- liftIO $ do
    -- This MVar is used to exchange information between the two processes below running in
    -- parallel, specifically to allow us to pass the results of re-compilation done by 'watch'
    -- into the 'onJobsQuietDown' handler used by 'startWebApp'.
    -- This way we can show newest Wasp compile warnings and errors (produced by recompilation from
    -- 'watch') once jobs from 'start' quiet down a bit.
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspProjectDir outDir ongoingCompilationResultMVar
    let startGeneratedWebApp =
          Wasp.Generator.start
            runConfigs
            waspProjectDir
            outDir
            (onJobsQuietDown ongoingCompilationResultMVar)
    -- In parallel:
    -- 1. watch for any changes in the Wasp project, be it users wasp code or users JS/HTML/...
    --    code. On any change, Wasp is recompiled (and generated app is re-generated).
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

makeDevAppUrls :: AppSpec -> StartArgs -> Command (AppComponentUrl, AppComponentUrl)
makeDevAppUrls appSpec args = do
  (clientPort, serverPort) <- findAppComponentPorts (args.clientPort, args.serverPort)
  return
    ( (makeDefaultDevClientUrl appSpec) {port = clientPort},
      defaultDevServerUrl {port = serverPort}
    )

makeDevRunConfigs ::
  Path' Abs (Dir WaspProjectDir) ->
  (AppComponentUrl, AppComponentUrl) ->
  Command (WebAppRunConfig, ServerRunConfig)
makeDevRunConfigs waspProjectDir (clientUrl, serverUrl) = do
  clientEnvVarsWithCtx <- liftIO $ getEnvVarsWithCtx Env.dotEnvClient
  serverEnvVarsWithCtx <- liftIO $ getEnvVarsWithCtx Env.dotEnvServer

  -- We only use this to check for errors. We throw away the resulting env vars,
  -- because the generated apps will read the .env files and inherited
  -- environment themselves.
  _ <- clientRunConfig `addEnvVarsUniqueC` clientEnvVarsWithCtx
  _ <- serverRunConfig `addEnvVarsUniqueC` serverEnvVarsWithCtx

  return (clientRunConfig, serverRunConfig)
  where
    (clientRunConfig, serverRunConfig) = makeRunConfigs clientUrl serverUrl

    getEnvVarsWithCtx dotEnvFile =
      mconcat
        [ EnvVarCtx.fromProjectFile waspProjectDir dotEnvFile,
          EnvVarCtx.fromCurrentEnvironment
        ]
