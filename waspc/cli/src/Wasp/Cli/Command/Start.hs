module Wasp.Cli.Command.Start
  ( start,
  )
where

import Control.Concurrent.Async (race)
import Control.Concurrent.MVar (MVar, newMVar, tryTakeMVar)
import Control.Monad (forM, unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (toList)
import Data.List (intercalate, nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, isJust)
import StrongPath (Abs, Dir, Path', fromRelFile, (</>))
import System.Environment (lookupEnv)
import Text.Printf (printf)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compile, printWarningsAndErrorsIfAny)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.News (fetchAndListMustSeeNewsIfDue)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Watch (watch)
import Wasp.Cli.Util.Apps (defaultAppPorts, getWaspEnvVars)
import qualified Wasp.Generator
import qualified Wasp.Generator.ServerGenerator.Common as Server
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning)
import Wasp.Project.Apps (Apps (..))
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import qualified Wasp.Project.Env as Env
import Wasp.Util.Terminal (styleCode)

-- | Does initial compile of wasp code and then runs the generated project.
-- It also listens for any file changes and recompiles and restarts generated project accordingly.
start :: Command ()
start = do
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

  throwIfWaspOwnedEnvVarsAreSet waspProjectDir
  let ports = defaultAppPorts

  cliSendMessageC $ Msg.Start "Starting compilation and setup phase. Hold tight..."

  (warnings, appSpec) <- compile

  DbConnectionEstablished <- require

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
    let startGeneratedWebApp =
          Wasp.Generator.start (getWaspEnvVars appSpec ports) waspProjectDir outDir (onJobsQuietDown ongoingCompilationResultMVar)
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

-- | The env vars Wasp derives from the ports it picked and injects into the processes
-- it starts. We read the names off the injection itself, so the two can't drift apart.
--
-- The ports and URLs we pass in are throwaway: which vars get injected never depends
-- on their values, only the names matter here.
waspOwnedDevEnvVarNames :: Apps [String]
waspOwnedDevEnvVarNames = map fst <$> (envVarMakers <*> pure irrelevantLocations)
  where
    envVarMakers =
      Apps
        { client = WebApp.getDevClientEnvVars,
          server = Server.getDevServerEnvVars
        }
    irrelevantLocations = pure (0, "")

-- | Wasp injects the env vars above into the processes it starts, and injected values
-- win over whatever the user wrote down. A value the user set would therefore be
-- silently ignored, so we stop instead.
throwIfWaspOwnedEnvVarsAreSet :: Path' Abs (Dir WaspProjectDir) -> Command ()
throwIfWaspOwnedEnvVarsAreSet waspProjectDir = do
  dotEnvVars <- liftIO $ Env.readDotEnvFiles waspProjectDir
  envVarsSetByUser <-
    liftIO $
      fmap (mergeSourcesPerEnvVar . concat . toList) $
        sequenceA $
          findEnvVarsSetByUser <$> waspOwnedDevEnvVarNames <*> Env.dotEnvFiles <*> dotEnvVars

  unless (null envVarsSetByUser) $
    throwError $
      CommandError "Wasp controls some of the env vars you set" $
        intercalate "\n" $
          [ printf
              "Wasp figures out the app's ports and URLs itself when you run %s, so it would ignore the values you set:"
              (styleCode "wasp start"),
            ""
          ]
            ++ map describeEnvVarSetByUser envVarsSetByUser
            ++ [ "",
                 "Remove them, and let Wasp manage the ports and URLs for you."
               ]
  where
    -- The client and the server take their port through the same env var name, so a
    -- var can turn up once per app. We report it once, listing everywhere it came from.
    mergeSourcesPerEnvVar =
      Map.toAscList . Map.fromListWith (\newSources sources -> nub $ sources ++ newSources)

    findEnvVarsSetByUser envVarNames dotEnvFile dotEnvFileVars =
      fmap catMaybes $
        forM envVarNames $ \envVarName -> do
          isSetInShellEnv <- isJust <$> lookupEnv envVarName
          let isSetInDotEnvFile = any ((== envVarName) . fst) dotEnvFileVars
              sources =
                [fromRelFile dotEnvFile | isSetInDotEnvFile]
                  ++ ["your environment" | isSetInShellEnv]
          return $ if null sources then Nothing else Just (envVarName, sources)

    describeEnvVarSetByUser (envVarName, sources) =
      printf "  - %s, set in %s" (styleCode envVarName) (intercalate " and " sources)
