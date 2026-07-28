module Wasp.Generator.NpmInstall
  ( installNpmDependenciesWithInstallRecord,
    installProjectNpmDependencies,
  )
where

import Control.Concurrent (Chan, newChan, threadDelay)
import Control.Concurrent.Async (concurrently)
import qualified Control.Concurrent.Async as Async
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate, release)
import Data.Functor ((<&>))
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Monad (GeneratorError (..))
import Wasp.Generator.NpmInstall.Common (AllNpmDeps (..), getAllNpmDeps)
import Wasp.Generator.NpmInstall.InstalledNpmDepsLog (forgetInstalledNpmDepsLog, loadInstalledNpmDepsLog, saveInstalledNpmDepsLog)
import qualified Wasp.Job as Job
import Wasp.Job.Internal (JobOutputSink, getJobOutputSink, writeJobOutput)
import qualified Wasp.Job.Node as Node
import qualified Wasp.Job.Output as Job.Output
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util (secondsToMicroSeconds)
import qualified Wasp.Util.IO as IOUitl

-- Runs `npm install` in the user's Wasp project directory.
-- Thanks to npm workspaces, this single install covers the user's project deps,
-- the generated server and web app deps, and the Wasp SDK.
installNpmDependenciesWithInstallRecord ::
  AppSpec ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO (Either GeneratorError ())
installNpmDependenciesWithInstallRecord spec dstDir = runExceptT $ do
  messagesChan <- liftIO newChan

  let allNpmDeps = getAllNpmDeps spec

  shouldInstallNpmDeps <-
    liftIO $
      or
        <$> sequence
          [ -- Users might by accident delete node_modules dir, so we check if it exists
            -- before assuming that we don't need to install npm deps.
            not <$> doesNodeModulesDirExist waspProjectDirPath,
            areThereNpmDepsToInstall allNpmDeps dstDir
          ]

  when shouldInstallNpmDeps $ do
    -- In case anything fails during installation that would leave node modules in
    -- a broken state, we remove the log of installed npm deps before we start npm install.
    liftIO $ forgetInstalledNpmDepsLog dstDir

    liftIO (installProjectNpmDependencies messagesChan waspProjectDirPath)
      >>= onLeftThrowError

    liftIO $ saveInstalledNpmDepsLog allNpmDeps dstDir
  where
    onLeftThrowError =
      either (\e -> throwError $ GenericGeneratorError $ "npm install failed: " ++ e) pure

    waspProjectDirPath = waspProjectDir spec

-- Installs npm dependencies from the user's package.json, by running `npm install` .
installProjectNpmDependencies ::
  Chan Job.JobEvent -> SP.Path SP.System Abs (Dir WaspProjectDir) -> IO (Either String ())
installProjectNpmDependencies messagesChan projectDir =
  Job.Output.printEventsPrefixedUntilExit messagesChan `concurrently` Job.runJob installProjectDepsJob messagesChan
    <&> \case
      (_, ExitFailure code) -> Left $ "Project setup failed with exit code " ++ show code ++ "."
      (_, ExitSuccess) -> Right ()
  where
    installProjectDepsJob =
      Job.makeJob Job.Wasp $
        installNpmDependenciesAndReport $
          Node.run projectDir "npm" ["install"]

installNpmDependenciesAndReport :: Job.JobAction a -> Job.JobAction a
installNpmDependenciesAndReport install = do
  Job.emitJobOutput Job.Stdout "Starting npm install\n"
  outputSink <- getJobOutputSink
  (progressReporterKey, _) <- allocate (Async.async $ reportInstallationProgress outputSink) Async.cancel
  result <- install
  release progressReporterKey
  return result

reportInstallationProgress :: JobOutputSink -> IO ()
reportInstallationProgress outputSink = reportPeriodically allPossibleMessages
  where
    reportPeriodically messages = do
      threadDelay $ secondsToMicroSeconds 5
      writeJobOutput outputSink Job.Stdout $ T.append (head messages) "\n"
      threadDelay $ secondsToMicroSeconds 5
      reportPeriodically $ drop 1 messages
    allPossibleMessages =
      cycle
        [ "Still installing npm dependencies!",
          "Installation going great - we'll get there soon!",
          "The installation is taking a while, but we'll get there!",
          "Yup, still not done installing.",
          "We're getting closer and closer, everything will be installed soon!",
          "Still waiting for the installation to finish? You should! We got too far to give up now!",
          "You've been waiting so patiently, just wait a little longer (for the installation to finish)..."
        ]

-- | Figure out if installation of npm deps is needed, be it for npm workspace deps (top level
-- package.json + web app + server), or for wasp sdk npm deps.
--
-- To this end, this code keeps track of the dependencies installed with a metadata file, which it
-- updates after each install.
--
-- Note: Here, we do a single check for all the deps, as the npm workspace ensures `npm install`
-- takes care of the user project, server, and web-app, all at once. The SDK is also installed as
-- part of our installation logic, so we don't need to check it separately.
areThereNpmDepsToInstall :: AllNpmDeps -> Path' Abs (Dir GeneratedAppDir) -> IO Bool
areThereNpmDepsToInstall allNpmDeps dstDir = do
  installedNpmDeps <- loadInstalledNpmDepsLog dstDir
  return $ installedNpmDeps /= Just allNpmDeps

doesNodeModulesDirExist :: Path' Abs (Dir WaspProjectDir) -> IO Bool
doesNodeModulesDirExist waspProjectDirPath = IOUitl.doesDirectoryExist nodeModulesDirInWaspProjectDirAbs
  where
    nodeModulesDirInWaspProjectDirAbs = waspProjectDirPath SP.</> nodeModulesDirInWaspProjectDir
