module Wasp.Generator.NpmInstall
  ( installNpmDependenciesWithInstallRecord,
  )
where

import Control.Concurrent (Chan, newChan, readChan, threadDelay, writeChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import Data.Functor ((<&>))
import qualified Data.Text as T
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import UnliftIO (race)
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Job (Job, JobMessage, JobType)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO.PrefixedWriter (PrefixedWriter, printJobMessagePrefixed, runPrefixedWriter)
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import qualified Wasp.Generator.NpmDependencies as N
import qualified Wasp.Generator.SdkGenerator as SdkGenerator
import Wasp.Generator.ServerGenerator as SG
import qualified Wasp.Generator.ServerGenerator.Setup as ServerSetup
import Wasp.Generator.WebAppGenerator as WG
import qualified Wasp.Generator.WebAppGenerator.Setup as WebAppSetup
import Wasp.Project.Common (WaspProjectDir)

-- Runs `npm install` for:
--   1. User's Wasp project (based on their package.json).
--   2. Wasp's generated webapp project.
--   3. Wasp's generated server project.
-- (1) runs first, (2) and (3) run concurrently after it.
-- It collects the output produced by these commands to pass them along to IO with a prefix.
installNpmDependenciesWithInstallRecord ::
  AppSpec ->
  Path' Abs (Dir ProjectRootDir) ->
  IO ([GeneratorWarning], [GeneratorError])
installNpmDependenciesWithInstallRecord spec dstDir = do
  messagesChan <- newChan

  installProjectNpmDependencies messagesChan (waspProjectDir spec) >>= \case
    Left npmInstallError -> do
      return ([], [GenericGeneratorError $ "npm install failed: " ++ npmInstallError])
    Right () -> installWebAppAndServerNpmDependenciesIfNeeded messagesChan
  where
    installWebAppAndServerNpmDependenciesIfNeeded messagesChan = do
      -- For webapp and server deps, we have this file where we write down what we previously installed,
      -- so we can skip `npm install` if nothing changed since then.
      -- Notice we don't currently have this for project/user deps, but could have.
      isWaspNpmInstallNeeded spec dstDir >>= \case
        Left errorMessage -> return ([], [GenericGeneratorError errorMessage])
        Right maybeFullStackDeps -> case maybeFullStackDeps of
          Nothing -> return ([], [])
          Just fullStackDeps -> do
            -- In case anything fails during installation that would leave node modules in
            -- a broken state, we remove the file before we start npm install.
            fileExists <- doesFileExist dependenciesInstalledFp
            when fileExists $ removeFile dependenciesInstalledFp
            installWebAppAndServerNpmDependencies messagesChan dstDir >>= \case
              Left npmInstallError -> do
                return ([], [GenericGeneratorError $ "npm install failed: " ++ npmInstallError])
              Right () -> do
                -- On successful npm install, record what we installed.
                B.writeFile dependenciesInstalledFp (Aeson.encode fullStackDeps)
                return ([], [])

    dependenciesInstalledFp = SP.fromAbsFile $ dstDir </> installedFullStackWaspNpmDependenciesFileInProjectRootDir

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
      installNpmDependenciesAndReport
        (SdkGenerator.installNpmDependencies projectDir)
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

-- Install npm dependencies for the Wasp's generated webapp and server projects.
installWebAppAndServerNpmDependencies ::
  Chan JobMessage -> SP.Path SP.System Abs (Dir ProjectRootDir) -> IO (Either String ())
installWebAppAndServerNpmDependencies messagesChan dstDir =
  handleSetupJobsMessages messagesChan
    `concurrently` (installServerDepsJob `concurrently` installWebAppDepsJob)
    <&> snd
    <&> \case
      (ExitSuccess, ExitSuccess) -> Right ()
      exitCodes -> Left $ setupFailedMessage exitCodes
  where
    installServerDepsJob = installNpmDependenciesAndReport (ServerSetup.installNpmDependencies dstDir) messagesChan J.Server
    installWebAppDepsJob = installNpmDependenciesAndReport (WebAppSetup.installNpmDependencies dstDir) messagesChan J.WebApp

    handleSetupJobsMessages = runPrefixedWriter . processMessages (False, False)
      where
        processMessages :: (Bool, Bool) -> Chan J.JobMessage -> PrefixedWriter ()
        processMessages (True, True) _ = return ()
        processMessages (isWebAppDone, isServerDone) chan = do
          jobMsg <- liftIO $ readChan chan
          case J._data jobMsg of
            J.JobOutput {} ->
              printJobMessagePrefixed jobMsg
                >> processMessages (isWebAppDone, isServerDone) chan
            J.JobExit {} -> case J._jobType jobMsg of
              J.WebApp -> processMessages (True, isServerDone) chan
              J.Server -> processMessages (isWebAppDone, True) chan
              J.Db -> error "This should never happen. No Db job should be active."
              J.Wasp -> error "This should never happen. No Wasp job should be active."

    setupFailedMessage (serverExitCode, webAppExitCode) =
      let serverErrorMessage = case serverExitCode of
            ExitFailure code -> " Server setup failed with exit code " ++ show code ++ "."
            _success -> ""
          webAppErrorMessage = case webAppExitCode of
            ExitFailure code -> " Web app setup failed with exit code " ++ show code ++ "."
            _success -> ""
       in "Setup failed!" ++ serverErrorMessage ++ webAppErrorMessage

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
      reportPeriodically (if hasLessThan2Elems messages then messages else drop 1 messages)
    secToMicroSec = (* 1000000)
    hasLessThan2Elems = null . drop 1
    allPossibleMessages =
      [ "Still installing npm dependencies!",
        "Installation going great - we'll get there soon!",
        "The installation is taking a while, but we'll get there!",
        "Yup, still not done installing.",
        "We're getting closer and closer, everything will be installed soon!",
        "Still waiting for the installation to finish? You should! We got too far to give up now!",
        "You've been waiting so patiently, just wait a little longer (for the installation to finish)..."
      ]

-- | Figure out if installation of npm deps for Wasp code (web app, server) is needed.
--
-- Redundant npm installs can be avoided if the dependencies specified
-- by wasp have not changed since the last time this ran.
--
-- To this end, this code keeps track of the dependencies installed with a metadata file, which
-- it updates after each install.
isWaspNpmInstallNeeded :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Either String (Maybe N.NpmDepsForFullStack))
isWaspNpmInstallNeeded spec dstDir = do
  let errorOrNpmDepsForFullStack = N.buildNpmDepsForFullStack spec (SG.npmDepsForWasp spec) (WG.npmDepsForWasp spec)
  case errorOrNpmDepsForFullStack of
    Left message -> return $ Left $ "determining npm deps to install failed: " ++ message
    Right npmDepsForFullStack -> do
      isInstallNeeded <- isWaspNpmInstallDifferent npmDepsForFullStack dstDir
      return $
        Right $
          if isInstallNeeded
            then Just npmDepsForFullStack
            else Nothing

-- Returns True only if the stored wasp's full stack dependencies are different from the
-- the full stack dependencies in the argument. If an installation record is missing
-- then it's always different.
isWaspNpmInstallDifferent :: N.NpmDepsForFullStack -> Path' Abs (Dir ProjectRootDir) -> IO Bool
isWaspNpmInstallDifferent appSpecFullStackNpmDependencies dstDir = do
  installedFullStackNpmDependencies <- loadInstalledFullStackWaspNpmDependencies dstDir
  return $ Just appSpecFullStackNpmDependencies /= installedFullStackNpmDependencies

-- TODO: we probably want to put this in a `waspmeta` directory in the future
installedFullStackWaspNpmDependenciesFileInProjectRootDir :: Path' (Rel ProjectRootDir) File'
installedFullStackWaspNpmDependenciesFileInProjectRootDir = [relfile|installedFullStackNpmDependencies.json|]

-- Load the record of the Wasp's (webapp + server) npm dependencies we installed from disk.
loadInstalledFullStackWaspNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe N.NpmDepsForFullStack)
loadInstalledFullStackWaspNpmDependencies dstDir = do
  let dependenciesInstalledFp = SP.fromAbsFile $ dstDir </> installedFullStackWaspNpmDependenciesFileInProjectRootDir
  fileExists <- doesFileExist dependenciesInstalledFp
  if fileExists
    then do
      fileContents <- B.readFile dependenciesInstalledFp
      return (Aeson.decode fileContents :: Maybe N.NpmDepsForFullStack)
    else return Nothing
