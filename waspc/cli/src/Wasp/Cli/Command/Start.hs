module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as Set
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (compile, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.News (fetchAndListMustSeeNewsIfDue)
import Wasp.Cli.Command.Require (DbConnectionEstablished (DbConnectionEstablished), InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Start.ArgumentsParser (StartArgs (..), startArgsParser)
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Env (EnvVar)
import qualified Wasp.Generator
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.Env (readDotEnvClient, readDotEnvServer)
import qualified Wasp.Util.Network.LocalAddress as LocalAddress

-- | Env var name read by the Vite plugin template to extend `server.allowedHosts`.
-- Keep in sync with the generated waspConfig.ts plugin.
lanAllowedHostEnvVarName :: String
lanAllowedHostEnvVarName = "WASP_LAN_ALLOWED_HOST"

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Arguments -> Command ()
start = withArguments "wasp start" startArgsParser $ \args -> do
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

  warnings <- compile

  DbConnectionEstablished <- require

  startOptions <- resolveStartOptions args waspProjectDir

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
    let startGeneratedWebApp = Wasp.Generator.start startOptions waspProjectDir outDir (onJobsQuietDown ongoingCompilationResultMVar)
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

resolveStartOptions :: StartArgs -> Path' Abs (Dir WaspProjectDir) -> Command Wasp.Generator.StartOptions
resolveStartOptions args waspProjectDir
  | disableLanExposure args = do
      cliSendMessageC $ Msg.Info "Local network exposure disabled (--no-lan)."
      return Wasp.Generator.defaultStartOptions
  | otherwise = do
      maybeHost <- liftIO $ resolveLanHost (hostOverride args)
      case maybeHost of
        Nothing -> do
          cliSendMessageC $
            Msg.Info "Could not detect a local network address; the app will only be reachable at http://localhost."
          return Wasp.Generator.defaultStartOptions
        Just host -> buildLanStartOptions host waspProjectDir

resolveLanHost :: Maybe String -> IO (Maybe String)
resolveLanHost (Just userHost) = return $ Just $ LocalAddress.toNipIoHostname userHost
resolveLanHost Nothing = do
  maybeIp <- LocalAddress.getLocalNetworkIPv4
  return $ LocalAddress.toNipIoHostname <$> maybeIp

buildLanStartOptions :: String -> Path' Abs (Dir WaspProjectDir) -> Command Wasp.Generator.StartOptions
buildLanStartOptions host waspProjectDir = do
  serverDotEnvVars <- liftIO $ readDotEnvServer waspProjectDir
  clientDotEnvVars <- liftIO $ readDotEnvClient waspProjectDir

  let candidateServerEnv =
        [ (Server.clientUrlEnvVarName, clientUrl),
          (Server.serverUrlEnvVarName, serverUrl)
        ]
      candidateClientEnv =
        [ (WebApp.serverUrlEnvVarName, serverUrl),
          (lanAllowedHostEnvVarName, host)
        ]

      (serverEnv, skippedServerVars) = skipExistingVars candidateServerEnv serverDotEnvVars
      (clientEnv, skippedClientVars) = skipExistingVars candidateClientEnv clientDotEnvVars

  warnAboutSkippedVars ".env.server" skippedServerVars
  warnAboutSkippedVars ".env.client" skippedClientVars

  cliSendMessageC $
    Msg.Info $
      unlines
        [ "Local network access enabled. Open this URL on another device on the same network:",
          "  " ++ clientUrl,
          "Use `wasp start --no-lan` to disable, or `wasp start --host <hostname>` to override the auto-detected hostname."
        ]

  return $
    Wasp.Generator.StartOptions
      { Wasp.Generator.extraServerEnv = serverEnv,
        Wasp.Generator.extraClientEnv = clientEnv
      }
  where
    clientUrl = "http://" ++ host ++ ":" ++ show WebApp.defaultClientPort
    serverUrl = "http://" ++ host ++ ":" ++ show Server.defaultServerPort

skipExistingVars :: [EnvVar] -> [EnvVar] -> ([EnvVar], [String])
skipExistingVars candidates existing = (kept, dropped)
  where
    existingNames = Set.fromList (map fst existing)
    (dropped, kept) = foldr partition ([], []) candidates
    partition var@(name, _) (skip, keep)
      | name `Set.member` existingNames = (name : skip, keep)
      | otherwise = (skip, var : keep)

warnAboutSkippedVars :: String -> [String] -> Command ()
warnAboutSkippedVars sourceLabel names =
  forM_ names $ \name ->
    cliSendMessageC $
      Msg.Warning "LAN env var skipped" $
        "Not injecting `"
          ++ name
          ++ "` because it is already set in `"
          ++ sourceLabel
          ++ "`. The app may not be reachable on the local network unless you update its value."
