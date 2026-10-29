module Wasp.Generator.NpmInstall
  ( installNpmDependenciesWithInstallRecord,
    installProjectNpmDependencies,
  )
where

import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import UnliftIO (race)
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Monad (GeneratorError (..))
import Wasp.Generator.NpmInstall.Common (AllNpmDeps (..), getAllNpmDeps)
import Wasp.Generator.NpmInstall.InstalledNpmDepsLog (forgetInstalledNpmDepsLog, loadInstalledNpmDepsLog, saveInstalledNpmDepsLog)
import Wasp.Job (Job, JobMessage, JobType)
import qualified Wasp.Job as J
import Wasp.Job.IO.PrefixedWriter (PrefixedWriter, printJobMessagePrefixed, runPrefixedWriter)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

-- Installs the npm deps of the user's project and its workspaces (the generated server and the
-- Wasp SDK). Skips the install if `node_modules` exists and the installed npm deps log matches
-- the current deps. Saves the log after a successful install.
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

-- Runs `npm install` in the user's Wasp project directory, which also installs the deps of all of
-- its npm workspaces (the generated server and the Wasp SDK).
installProjectNpmDependencies ::
  Chan JobMessage -> SP.Path SP.System Abs (Dir WaspProjectDir) -> IO (Either String ())
installProjectNpmDependencies messagesChan projectDir = do
  (_, installExitCode) <- handleProjectInstallMessages messagesChan `concurrently` installProjectDepsJob
  return $ case installExitCode of
    ExitFailure code -> Left $ "Project setup failed with exit code " ++ show code ++ "."
    _success -> Right ()
  where
    installProjectDepsJob =
      installNpmDependenciesAndReport
        (runNodeCommandAsJob projectDir "npm" ["install"] J.Wasp)
        messagesChan
        J.Wasp
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
reportInstallationProgress chan jobType =
  mapM_ reportMessage $ cycle possibleMessages
  where
    reportMessage message = do
      threadDelay $ secToMicroSec 5
      writeChan chan $ J.JobMessage {J._data = J.JobOutput (T.append message "\n") J.Stdout, J._jobType = jobType}
      threadDelay $ secToMicroSec 5

    secToMicroSec = (* 1000000)

    possibleMessages =
      [ "Still installing npm dependencies!",
        "Installation going great - we'll get there soon!",
        "The installation is taking a while, but we'll get there!",
        "Yup, still not done installing.",
        "We're getting closer and closer, everything will be installed soon!",
        "Still waiting for the installation to finish? You should! We got too far to give up now!",
        "You've been waiting so patiently, just wait a little longer (for the installation to finish)..."
      ]

-- | Checks if the deps of the user's project, the generated server, and the Wasp SDK differ from
-- the ones recorded in the installed npm deps log during the last successful install.
--
-- Note: A single check covers all of them because the server and the SDK are npm workspaces of the
-- user's project, so one `npm install` installs them all at once.
areThereNpmDepsToInstall :: AllNpmDeps -> Path' Abs (Dir GeneratedAppDir) -> IO Bool
areThereNpmDepsToInstall allNpmDeps dstDir = do
  installedNpmDeps <- loadInstalledNpmDepsLog dstDir
  return $ installedNpmDeps /= Just allNpmDeps

doesNodeModulesDirExist :: Path' Abs (Dir WaspProjectDir) -> IO Bool
doesNodeModulesDirExist waspProjectDirPath = IOUtil.doesDirectoryExist nodeModulesDirInWaspProjectDirAbs
  where
    nodeModulesDirInWaspProjectDirAbs = waspProjectDirPath SP.</> nodeModulesDirInWaspProjectDir
