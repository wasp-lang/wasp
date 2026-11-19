module Wasp.Cli.Command.CreateNewProject.InstallDependencies
  ( installProjectDependencies,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.Text as T
import Data.Void (Void, absurd)
import StrongPath (Abs, Dir, Path')
import System.Exit (ExitCode (..))
import qualified Wasp.Job as Job
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.PackageManager (PackageManager, getPackageManagerName, makeInstallCommand)
import Wasp.Util (checkExitSuccess, secondsToMicroSeconds)

-- | Installs the project's packages with its package manager, and reports
-- progress while it runs.
installProjectDependencies :: PackageManager -> Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installProjectDependencies packageManager projectDir =
  Job.run (Job.prefixWith Job.Wasp $ installDependenciesAndReport packageManager projectDir)
    <&> checkExitSuccess "Project setup"

installDependenciesAndReport :: PackageManager -> Path' Abs (Dir WaspProjectDir) -> Job.Job ExitCode
installDependenciesAndReport packageManager projectDir = do
  Job.emitOutput Job.Stdout $ T.pack $ "Starting " ++ makeInstallCommand packageManager ++ "\n"

  either absurd id
    <$> Job.race
      reportInstallationProgress
      ( Job.fromProc
          $ Job.setCwd projectDir
          $ Job.proc (getPackageManagerName packageManager) ["install"]
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
      [ "Still installing dependencies!",
        "Installation going great - we'll get there soon!",
        "The installation is taking a while, but we'll get there!",
        "Yup, still not done installing.",
        "We're getting closer and closer, everything will be installed soon!",
        "Still waiting for the installation to finish? You should! We got too far to give up now!",
        "You've been waiting so patiently, just wait a little longer (for the installation to finish)..."
      ]
