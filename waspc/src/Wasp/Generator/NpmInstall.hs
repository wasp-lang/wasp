module Wasp.Generator.NpmInstall
  ( installNpmDependenciesWithInstallRecord,
    installProjectNpmDependencies,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Monad (GeneratorError (..))
import Wasp.Generator.NpmInstall.Common (AllNpmDeps (..), getAllNpmDeps)
import Wasp.Generator.NpmInstall.InstalledNpmDepsLog (forgetInstalledNpmDepsLog, loadInstalledNpmDepsLog, saveInstalledNpmDepsLog)
import qualified Wasp.Job as Job
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util (checkExitSuccess, secondsToMicroSeconds)
import qualified Wasp.Util.IO as IOUtil

-- Installs the npm deps of the user's project and its workspaces (the generated server and the
-- Wasp SDK). Skips the install if `node_modules` exists and the installed npm deps log matches
-- the current deps. Saves the log after a successful install.
installNpmDependenciesWithInstallRecord ::
  AppSpec ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO (Either GeneratorError ())
installNpmDependenciesWithInstallRecord spec dstDir = runExceptT $ do
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

    liftIO (installProjectNpmDependencies waspProjectDirPath)
      >>= onLeftThrowError

    liftIO $ saveInstalledNpmDepsLog allNpmDeps dstDir
  where
    onLeftThrowError =
      either (\e -> throwError $ GenericGeneratorError $ "npm install failed: " ++ e) pure

    waspProjectDirPath = waspProjectDir spec

-- Runs `npm install` in the user's Wasp project directory, which also installs the deps of all of
-- its npm workspaces (the generated server and the Wasp SDK).
installProjectNpmDependencies ::
  SP.Path SP.System Abs (Dir WaspProjectDir) -> IO (Either String ())
installProjectNpmDependencies projectDir =
  Job.run (Job.prefixWith Job.Wasp $ installNpmDependenciesAndReport projectDir)
    <&> checkExitSuccess "Project setup"

{- HLINT ignore installNpmDependenciesAndReport "Avoid npm and npx" -}

installNpmDependenciesAndReport :: Path' Abs (Dir WaspProjectDir) -> Job.Job ExitCode
installNpmDependenciesAndReport projectDir = do
  Job.emitOutput Job.Stdout "Starting npm install\n"

  either absurd id
    <$> Job.race
      reportInstallationProgress
      ( Job.fromProc
          $ Job.setCwd projectDir
          $ Job.proc "npm" ["install"]
      )

reportInstallationProgress :: Job.Job Void
reportInstallationProgress =
  forever $ mapM_ reportMessage possibleMessages
  where
    reportMessage message = do
      liftIO $ threadDelay $ secondsToMicroSeconds 5
      Job.emitOutput Job.Stdout $ T.append message "\n"
      liftIO $ threadDelay $ secondsToMicroSeconds 5

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
