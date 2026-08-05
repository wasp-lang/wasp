module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (compile, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.News (fetchAndListMustSeeNewsIfDue)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Start.ArgumentsParser (StartArgs (..), startArgsParser)
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.Util.EnvVarInputs (resolveEnvVarInputs)
import qualified Wasp.Cli.Util.EnvVarInputs as EnvVarInputs
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Cli.Util.PortArgument (resolveAppPorts)
import Wasp.Cli.Util.Services (getDevUrlMakers, getWaspEnvVars)
import qualified Wasp.Generator
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import qualified Wasp.Project.Env as Env
import Wasp.Project.PerService (client, server)

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

  (warnings, appSpec) <- compile

  -- Nothing holds on to the ports we pick until the generated app binds them, so somebody
  -- else can take them in the meantime. We resolve them as late as we can (compiling can
  -- take a while, especially on the first run) to keep that window short.
  ports <- resolveAppPorts args.ports

  let urls = getDevUrlMakers appSpec <*> ports
      waspEnvVars = getWaspEnvVars appSpec ports
      envVarInputs = getEnvVarInputs <$> Env.dotEnvFiles

  envVars <-
    sequence $
      resolveEnvVarInputs waspProjectDir <$> waspEnvVars <*> envVarInputs

  DbConnectionEstablished <- require

  cliSendMessageC $ Msg.Start "Listening for file changes..."
  cliSendMessageC $ Msg.Start "Starting up generated project..."
  cliSendMessageC $
    Msg.Info $
      unlines
        -- The client's URL already ends with a slash (it's the app's base
        -- directory), so we add one to the server's to keep the pair consistent.
        [ " ℹ Client: " ++ urls.client,
          " ℹ Server: " ++ urls.server ++ "/"
        ]

  watchOrStartResult <- liftIO $ do
    -- This MVar is used to exchange information between the two processes below running in
    -- parallel, specifically to allow us to pass the results of re-compilation done by 'watch'
    -- into the 'onJobsQuietDown' handler used by 'startWebApp'.
    -- This way we can show newest Wasp compile warnings and errors (produced by recompilation from
    -- 'watch') once jobs from 'start' quiet down a bit.
    ongoingCompilationResultMVar <- newMVar (warnings, [])
    let watchWaspProjectSource = watch waspProjectDir outDir ongoingCompilationResultMVar
    let startGeneratedWebApp =
          Wasp.Generator.start envVars waspProjectDir outDir (onJobsQuietDown ongoingCompilationResultMVar)
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
    getEnvVarInputs file = [EnvVarInputs.FromProjectFile file, EnvVarInputs.Inherit]

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
