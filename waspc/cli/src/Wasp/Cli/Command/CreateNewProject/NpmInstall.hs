module Wasp.Cli.Command.CreateNewProject.NpmInstall
  ( installProjectNpmDependencies,
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
import Wasp.Util (checkExitSuccess, secondsToMicroSeconds)

-- | Runs `npm install` in the project, and reports progress while it runs.
installProjectNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
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
