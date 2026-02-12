module Wasp.Cli.Command.BuildStart
  ( buildStart,
  )
where

import Control.Concurrent.Async (concurrently)
import Control.Concurrent.Chan (newChan)
import Control.Lens
import Control.Monad.Except (ExceptT (ExceptT), MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Aeson (Value)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.List (isSuffixOf)
import StrongPath (Abs, Dir, Path', castRel, (</>))
import qualified System.FilePath as FP
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.BuildStart.ArgumentsParser (buildStartArgsParser)
import Wasp.Cli.Command.BuildStart.Client (buildClient, buildSsr, startClient)
import Wasp.Cli.Command.BuildStart.Config (BuildStartConfig, buildDir, makeBuildStartConfig)
import Wasp.Cli.Command.BuildStart.Server (buildServer, startServer)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (GeneratedCodeIsProduction (GeneratedCodeIsProduction), InWaspProject (InWaspProject))
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Job.Except (ExceptJob)
import qualified Wasp.Job.Except as ExceptJob
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( WaspProjectDir,
    getSrcTsConfigInWaspProjectDir,
    packageJsonInWaspProjectDir,
    packageLockJsonInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Util.IO (copyDirectory, copyFile)
import Wasp.Util.Json (updateJsonFile)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Page as Page

buildStart :: Arguments -> Command ()
buildStart = withArguments "wasp build start" buildStartArgsParser $ \args -> do
  GeneratedCodeIsProduction _ <- require

  InWaspProject waspProjectDir <- require
  appSpec <- analyze waspProjectDir

  -- TODO: Find a way to easily check we can connect to the DB. We'd like to
  -- throw a clear error if not available. (See #2858)
  --
  -- It is not a big problem right now, because Prisma will fail shortly after
  -- the server starts if the DB is not running anyway, and with a very clear
  -- error message that we print.

  config <- makeBuildStartConfig appSpec args waspProjectDir

  liftIO (prepareFilesNecessaryForDockerBuild waspProjectDir (buildDir config)) >>= \case
    Left err -> throwError $ CommandError "Failed to prepare files necessary for docker build" err
    Right () -> return ()

  let ssrEnabled = any ((== Just True) . Page.ssr . snd) (AS.getPages appSpec)
  buildAndStartServerAndClient config ssrEnabled

buildAndStartServerAndClient :: BuildStartConfig -> Bool -> Command ()
buildAndStartServerAndClient config ssrEnabled = do
  cliSendMessageC $ Msg.Start "Building client..."
  runAndPrintJob "Building client failed." $
    buildClient config
  cliSendMessageC $ Msg.Success "Client built."

  when ssrEnabled $ do
    cliSendMessageC $ Msg.Start "Building SSR bundle..."
    runAndPrintJob "Building SSR bundle failed." $
      buildSsr config
    cliSendMessageC $ Msg.Success "SSR bundle built."

  cliSendMessageC $ Msg.Start "Building server..."
  runAndPrintJob "Building server failed." $
    buildServer config
  cliSendMessageC $ Msg.Success "Server built."

  cliSendMessageC $ Msg.Start "Starting client and server..."
  runAndPrintJob "Starting Wasp app failed." $
    ExceptJob.race_
      (startClient config ssrEnabled)
      (startServer config)
  where
    runAndPrintJob :: String -> ExceptJob -> Command ()
    runAndPrintJob errorMessage job = do
      liftIO (runAndPrintJobIO job)
        >>= either (throwError . CommandError errorMessage) return

    runAndPrintJobIO :: ExceptJob -> IO (Either String ())
    runAndPrintJobIO job = do
      chan <- newChan
      (result, _) <-
        concurrently
          (runExceptT $ job chan)
          (readJobMessagesAndPrintThemPrefixed chan)
      return result

prepareFilesNecessaryForDockerBuild ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  IO (Either String ())
prepareFilesNecessaryForDockerBuild waspProjectDir buildDir' = runExceptT $ do
  waspFilePath <- ExceptT $ findWaspFile waspProjectDir
  let srcTsConfigPath = getSrcTsConfigInWaspProjectDir waspFilePath

  -- Until we implement the solution described in https://github.com/wasp-lang/wasp/issues/1769,
  -- we're copying all files and folders necessary for Docker build into the .wasp/out directory.
  -- We chose this approach for 0.12.0 (instead of building from the project root) because:
  --   - The Docker build context remains small (~1.5 MB vs ~900 MB).
  --   - We don't risk copying possible secrets from the project root into Docker's build context.
  --   - The commands for building the project stay the same as before
  --     0.12.0, which is good for both us (e.g., for fly deployment) and our
  --     users  (no changes in CI/CD scripts).
  -- For more details, read the issue linked above.
  liftIO $
    copyDirectory
      (waspProjectDir </> srcDirInWaspProjectDir)
      (buildDir' </> castRel srcDirInWaspProjectDir)

  let packageJsonInBuildDir = buildDir' </> castRel packageJsonInWaspProjectDir
  let packageLockJsonInBuildDir = buildDir' </> castRel packageLockJsonInWaspProjectDir
  let tsconfigJsonInBuildDir = buildDir' </> castRel srcTsConfigPath

  liftIO $
    copyFile
      (waspProjectDir </> packageJsonInWaspProjectDir)
      packageJsonInBuildDir

  liftIO $
    copyFile
      (waspProjectDir </> packageLockJsonInWaspProjectDir)
      packageLockJsonInBuildDir

  -- We need the main tsconfig.json file since the built server's TS config
  -- extends from it.
  liftIO $
    copyFile
      (waspProjectDir </> srcTsConfigPath)
      tsconfigJsonInBuildDir

  -- A hacky quick fix for https://github.com/wasp-lang/wasp/issues/2368
  -- We should remove this code once we implement a proper solution.
  ExceptT $ updateJsonFile removeWaspConfigFromDevDependenciesArray packageJsonInBuildDir
  ExceptT $ updateJsonFile removeAllMentionsOfWaspConfigInPackageLockJson packageLockJsonInBuildDir
  where
    removeAllMentionsOfWaspConfigInPackageLockJson :: Value -> Value
    removeAllMentionsOfWaspConfigInPackageLockJson packageLockJsonObject =
      -- We want to:
      --   1. Remove the `wasp-config` dev dependency from the root package in package-lock.json.
      --   This is at `packageLock["packages"][""]["wasp-config"]`.
      --   2. Remove all package location entries for the `wasp-config` package
      --   (i.e., entries whose location keys end in `/wasp-config`).
      --   Example locations include:
      --      packageLock["packages"]["../../data/packages/wasp-config"]
      --      packageLock["packages"]["node_modules/wasp-config"]
      --      packageLock["packages"]["/home/filip/../wasp-config"]
      packageLockJsonObject
        & key "packages" . key "" %~ removeWaspConfigFromDevDependenciesArray
        & key "packages" . _Object
          %~ KM.filterWithKey
            (\packageLocation _ -> not $ isWaspConfigPackageLocation (Key.toString packageLocation))

    isWaspConfigPackageLocation :: String -> Bool
    isWaspConfigPackageLocation packageLocation =
      (FP.pathSeparator : "wasp-config") `isSuffixOf` packageLocation

    removeWaspConfigFromDevDependenciesArray :: Value -> Value
    removeWaspConfigFromDevDependenciesArray original =
      original & key "devDependencies" . _Object . at "wasp-config" .~ Nothing
