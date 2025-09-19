module Wasp.Generator.NpmInstall
  ( installNpmDependenciesWithInstallRecord,
  )
where

import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad.Except (MonadError (throwError), runExceptT, when)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import UnliftIO (race)
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (..))
import Wasp.Generator.NpmInstall.Common (AllNpmDeps (..), getAllNpmDeps)
import Wasp.Generator.NpmInstall.InstalledNpmDepsLog (forgetInstalledNpmDepsLog, loadInstalledNpmDepsLog, saveInstalledNpmDepsLog)
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import Wasp.Job (Job, JobMessage, JobType)
import qualified Wasp.Job as J
import Wasp.Job.IO.PrefixedWriter (PrefixedWriter, printJobMessagePrefixed, runPrefixedWriter)
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.Util.IO as IOUitl

-- Runs `npm install` for:
--   1. User's Wasp project (based on their package.json): user deps.
--   2. Wasp's generated webapp project: wasp deps.
--   3. Wasp's generated server project: wasp deps.
-- (1) runs first, (2) and (3) run concurrently after it.
-- It collects the output produced by these commands to pass them along to IO with a prefix.
installNpmDependenciesWithInstallRecord ::
  AppSpec ->
  Path' Abs (Dir ProjectRootDir) ->
  IO (Either GeneratorError ())
installNpmDependenciesWithInstallRecord spec dstDir = runExceptT $ do
  messagesChan <- liftIO newChan

  allNpmDeps <- getAllNpmDeps spec & onLeftThrowError

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
  Chan JobMessage -> SP.Path SP.System Abs (Dir WaspProjectDir) -> IO (Either String ())
installProjectNpmDependencies messagesChan projectDir =
  handleProjectInstallMessages messagesChan `concurrently` installProjectDepsJob
    <&> snd
    <&> \case
      ExitFailure code -> Left $ "Project setup failed with exit code " ++ show code ++ "."
      _success -> Right ()
  where
    installProjectDepsJob =
      installNpmDependenciesAndReport (SdkGenerator.installNpmDependencies projectDir) messagesChan J.Wasp
    handleProjectInstallMessages :: Chan J.JobMessage -> IO ()
    handleProjectInstallMessages = runPrefixedWriter . processMessages
      where
        processMessages :: Chan J.JobMessage -> PrefixedWriter ()
        processMessages chan = do
          jobMsg <- liftIO $ readChan chan
          case J._data jobMsg of
            J.JobOutput {} -> printJobMessagePrefixed jobMsg >> processMessages chan
            J.JobExit {} -> return ()

installNpmDependenciesAndReport :: Job -> Chan JobMessage -> JobType -> IO ExitCode
installNpmDependenciesAndReport installJob chan jobType = do
  writeChan chan $ J.JobMessage {J._data = J.JobOutput "Starting npm install\n" J.Stdout, J._jobType = jobType}
  result <- installJob chan `race` reportInstallationProgress chan jobType
  case result of
    Left exitCode -> return exitCode
    Right _ -> error "This should never happen, reporting installation progress should run forever."

reportInstallationProgress :: Chan JobMessage -> JobType -> IO ()
reportInstallationProgress chan jobType = reportPeriodically allPossibleMessages
  where
    reportPeriodically messages = do
      threadDelay $ secToMicroSec 5
      writeChan chan $ J.JobMessage {J._data = J.JobOutput (T.append (head messages) "\n") J.Stdout, J._jobType = jobType}
      threadDelay $ secToMicroSec 5
      reportPeriodically $ drop 1 messages
    secToMicroSec = (* 1000000)
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
areThereNpmDepsToInstall :: AllNpmDeps -> Path' Abs (Dir ProjectRootDir) -> IO Bool
areThereNpmDepsToInstall allNpmDeps dstDir = do
  installedNpmDeps <- loadInstalledNpmDepsLog dstDir
  return $ installedNpmDeps /= Just allNpmDeps

doesNodeModulesDirExist :: Path' Abs (Dir WaspProjectDir) -> IO Bool
doesNodeModulesDirExist waspProjectDirPath = IOUitl.doesDirectoryExist nodeModulesDirInWaspProjectDirAbs
  where
    nodeModulesDirInWaspProjectDirAbs = waspProjectDirPath SP.</> nodeModulesDirInWaspProjectDir
