module Wasp.Generator.NpmInstall
  ( isNpmInstallNeeded,
    installNpmDependenciesWithInstallRecord,
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
import Wasp.AppSpec (AppSpec)
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

-- | Figure out if npm install is needed.
--
-- Redundant npm installs can be avoided if the dependencies specified
-- by the user and wasp have not changed since the last time this ran.
--
-- Npm instal is needed only if the dependencies described in the user wasp file are
-- different from the dependencies that we just installed. To this end, this
-- code keeps track of the dependencies installed with a metadata file, which
-- it updates after each install.
--
-- NOTE: we assume that the dependencies in package.json are the same as the
-- ones we derive from the AppSpec. We derive them the same way but it does
-- involve different code paths.
-- This module could work in an completely different way, independently
-- from AppSpec at all. It could work by ensuring a `npm install` is
-- consistent with a metadata file derived from `package.json` during its
-- previous run. This would be more decoupled from the rest of the system.
-- Npm conflict handling could be ignored in that case, because it would work
-- from the record of what's in package.json.
isNpmInstallNeeded :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO (Either String (Maybe N.NpmDepsForFullStack))
isNpmInstallNeeded spec dstDir = do
  let errorOrNpmDepsForFullStack = N.buildNpmDepsForFullStack spec (SG.npmDepsForWasp spec) (WG.npmDepsForWasp spec)
  case errorOrNpmDepsForFullStack of
    Left message -> return $ Left $ "determining npm deps to install failed: " ++ message
    Right npmDepsForFullStack -> do
      isInstallNeeded <- isNpmInstallDifferent npmDepsForFullStack dstDir
      return $
        Right $
          if isInstallNeeded
            then Just npmDepsForFullStack
            else Nothing

-- Run npm install for desired AppSpec dependencies, recording what we installed
-- Installation may fail, in which the installation record is removed.
installNpmDependenciesWithInstallRecord ::
  N.NpmDepsForFullStack ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  IO ([GeneratorWarning], [GeneratorError])
installNpmDependenciesWithInstallRecord npmDepsForFullStack waspProjectDir dstDir = do
  -- in case anything fails during installation that would leave node modules in
  -- a broken state, we remove the file before we start npm install
  fileExists <- doesFileExist dependenciesInstalledFp
  when fileExists $ removeFile dependenciesInstalledFp
  -- now actually do the installation
  npmInstallResult <- installNpmDependencies waspProjectDir dstDir
  case npmInstallResult of
    Left npmInstallError -> do
      return ([], [GenericGeneratorError $ "npm install failed: " ++ npmInstallError])
    Right () -> do
      -- on successful npm install, record what we installed
      B.writeFile dependenciesInstalledFp (Aeson.encode npmDepsForFullStack)
      return ([], [])
  where
    dependenciesInstalledFp = SP.fromAbsFile $ dstDir </> installedFullStackNpmDependenciesFileInProjectRootDir

-- Returns True only if the stored full stack dependencies are different from the
-- the full stack dependencies in the argument. If an installation record is missing
-- then it's always different.
isNpmInstallDifferent :: N.NpmDepsForFullStack -> Path' Abs (Dir ProjectRootDir) -> IO Bool
isNpmInstallDifferent appSpecFullStackNpmDependencies dstDir = do
  installedFullStackNpmDependencies <- loadInstalledFullStackNpmDependencies dstDir
  return $ Just appSpecFullStackNpmDependencies /= installedFullStackNpmDependencies

-- TODO: we probably want to put this in a `waspmeta` directory in the future
installedFullStackNpmDependenciesFileInProjectRootDir :: Path' (Rel ProjectRootDir) File'
installedFullStackNpmDependenciesFileInProjectRootDir = [relfile|installedFullStackNpmDependencies.json|]

-- Load the record of the dependencies we installed from disk.
loadInstalledFullStackNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe N.NpmDepsForFullStack)
loadInstalledFullStackNpmDependencies dstDir = do
  let dependenciesInstalledFp = SP.fromAbsFile $ dstDir </> installedFullStackNpmDependenciesFileInProjectRootDir
  fileExists <- doesFileExist dependenciesInstalledFp
  if fileExists
    then do
      fileContents <- B.readFile dependenciesInstalledFp
      return (Aeson.decode fileContents :: Maybe N.NpmDepsForFullStack)
    else return Nothing

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

installNpmDependenciesAndReport :: Job -> Chan JobMessage -> JobType -> IO ExitCode
installNpmDependenciesAndReport installJob chan jobType = do
  writeChan chan $ J.JobMessage {J._data = J.JobOutput "Starting npm install\n" J.Stdout, J._jobType = jobType}
  result <- installJob chan `race` reportInstallationProgress chan jobType
  case result of
    Left exitCode -> return exitCode
    Right _ -> error "This should never happen, reporting installation progress should run forever."

{- HLINT ignore installNpmDependencies "Redundant <$>" -}

-- Run the individual `npm install` commands for both server and webapp projects
-- It runs these concurrently, collects the output produced by these commands
-- to pass them along to IO with a prefix
installNpmDependencies :: Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
installNpmDependencies projectDir dstDir = do
  messagesChan <- newChan
  installProjectNpmDependencies messagesChan projectDir >>= \case
    ExitFailure code -> return $ Left $ "Project setup failed with exit code " ++ show code ++ "."
    _success -> do
      installWebAppAndServerNpmDependencies messagesChan dstDir <&> \case
        (ExitSuccess, ExitSuccess) -> Right ()
        exitCodes -> Left $ setupFailedMessage exitCodes
  where
    setupFailedMessage (serverExitCode, webAppExitCode) =
      let serverErrorMessage = case serverExitCode of
            ExitFailure code -> " Server setup failed with exit code " ++ show code ++ "."
            _success -> ""
          webAppErrorMessage = case webAppExitCode of
            ExitFailure code -> " Web app setup failed with exit code " ++ show code ++ "."
            _success -> ""
       in "Setup failed!" ++ serverErrorMessage ++ webAppErrorMessage

installProjectNpmDependencies ::
  Chan JobMessage -> SP.Path SP.System Abs (Dir WaspProjectDir) -> IO ExitCode
installProjectNpmDependencies messagesChan projectDir =
  snd <$> handleProjectInstallMessages messagesChan `concurrently` installProjectDepsJob
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

installWebAppAndServerNpmDependencies ::
  Chan JobMessage -> SP.Path SP.System Abs (Dir ProjectRootDir) -> IO (ExitCode, ExitCode)
installWebAppAndServerNpmDependencies messagesChan dstDir =
  snd <$> handleSetupJobsMessages messagesChan `concurrently` (installServerDepsJob `concurrently` installWebAppDepsJob)
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
