module Wasp.Generator.NpmInstall
  ( ensureNpmInstall,
  )
where

import Control.Concurrent (Chan, newChan, readChan)
import Control.Concurrent.Async (concurrently)
import Control.Monad (when)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesFileExist, removeFile)
import System.Exit (ExitCode (..))
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.Job as J
import Wasp.Generator.Job.IO (printPrefixedJobMessage)
import Wasp.Generator.Monad (GeneratorError (..), GeneratorWarning (..))
import qualified Wasp.Generator.NpmDependencies as N
import Wasp.Generator.ServerGenerator as SG
import qualified Wasp.Generator.ServerGenerator.Setup as ServerSetup
import Wasp.Generator.WebAppGenerator as WG
import qualified Wasp.Generator.WebAppGenerator.Setup as WebAppSetup

-- | Run npm install if needed
-- Redundant npm installs can be avoided if the dependencies specified
-- by the user and wasp have not changed since the last time this ran.

-- It only does this if the dependencies described in the user wasp file are
-- different from the dependencies that we just installed. To this end, this
-- code keeps track of the dependencies installed with a metadata file, which
-- it updates after each install.

-- NOTE: we assume that the dependencies in package.json are the same as the
-- ones we derive from the AppSpec. We derive them the same way but it does
-- involve different code paths.
-- This module could work in an completely different way, independently
-- from AppSpec at all. It could work by ensuring a `npm install` is
-- consistent with a metadata file derived from `package.json` during its
-- previous run. This would be more decoupled from the rest of the system.
-- Npm conflict handling could be ignored in that case, because it would work
-- from the record of what's in package.json.
ensureNpmInstall :: AppSpec -> Path' Abs (Dir ProjectRootDir) -> IO ([GeneratorWarning], [GeneratorError])
ensureNpmInstall spec dstDir = do
  let errorOrFullStackNpmDependencies = N.buildFullStackNpmDependencies spec SG.waspNpmDependencies WG.waspNpmDependencies
  case errorOrFullStackNpmDependencies of
    Left message -> return ([], [GenericGeneratorError ("npm install failed: " ++ message)])
    Right fullStackNpmDependencies -> do
      needed <- isNpmInstallDifferent fullStackNpmDependencies dstDir
      if needed
        then installNpmDependenciesWithInstallRecord fullStackNpmDependencies dstDir
        else return ([], [])

-- Run npm install for desired AppSpec dependencies, recording what we installed
-- Installation may fail, in which the installation record is removed.
installNpmDependenciesWithInstallRecord :: N.FullStackNpmDependencies -> Path' Abs (Dir ProjectRootDir) -> IO ([GeneratorWarning], [GeneratorError])
installNpmDependenciesWithInstallRecord appSpecFullStackNpmDependencies dstDir = do
  -- in case anything fails during installation that would leave node modules in
  -- a broken state, we remove the file before we start npm install
  fileExists <- doesFileExist dependenciesInstalledFp
  when fileExists $ removeFile dependenciesInstalledFp
  npmInstallResult <- installNpmDependencies dstDir
  case npmInstallResult of
    Left npmInstallError -> do
      return ([], [GenericGeneratorError $ "npm install failed: " ++ npmInstallError])
    Right () -> do
      -- on successful npm install, record what we installed
      B.writeFile dependenciesInstalledFp (Aeson.encode appSpecFullStackNpmDependencies)
      return ([], [])
  where
    dependenciesInstalledFp = SP.fromAbsFile $ dstDir </> installedFullStackNpmDependenciesFileInProjectRootDir

-- Returns True only if the stored full stack dependencies are different from the
-- the full stack dependencies in the argument. If an installation record is missing
-- then it's always different.
isNpmInstallDifferent :: N.FullStackNpmDependencies -> Path' Abs (Dir ProjectRootDir) -> IO Bool
isNpmInstallDifferent appSpecFullStackNpmDependencies dstDir = do
  installedFullStackNpmDependencies <- loadInstalledFullStackNpmDependencies dstDir
  return $ Just appSpecFullStackNpmDependencies /= installedFullStackNpmDependencies

-- TODO: we probably want to put this in a `waspmeta` directory in the future
installedFullStackNpmDependenciesFileInProjectRootDir :: Path' (Rel ProjectRootDir) File'
installedFullStackNpmDependenciesFileInProjectRootDir = [relfile|installedFullStackNpmDependencies.json|]

-- Load the record of the dependencies we installed from disk.
loadInstalledFullStackNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe N.FullStackNpmDependencies)
loadInstalledFullStackNpmDependencies dstDir = do
  let dependenciesInstalledFp = SP.fromAbsFile $ dstDir </> installedFullStackNpmDependenciesFileInProjectRootDir
  fileExists <- doesFileExist dependenciesInstalledFp
  if fileExists
    then do
      fileContents <- B.readFile dependenciesInstalledFp
      return (Aeson.decode fileContents :: Maybe N.FullStackNpmDependencies)
    else return Nothing

-- Run the individual `npm install` commands for both server and webapp projects
-- It runs these concurrently, collects the output produced by these commands
-- to pass them along to IO with a prefix
installNpmDependencies :: Path' Abs (Dir ProjectRootDir) -> IO (Either String ())
installNpmDependencies projectDir = do
  chan <- newChan
  let runSetupJobs = concurrently (ServerSetup.installNpmDependencies projectDir chan) (WebAppSetup.installNpmDependencies projectDir chan)
  (_, result) <- concurrently (handleJobMessages chan) runSetupJobs
  case result of
    (ExitSuccess, ExitSuccess) -> return $ Right ()
    exitCodes -> return $ Left $ setupFailedMessage exitCodes
  where
    handleJobMessages = go Nothing (False, False)
      where
        go :: Maybe J.JobMessage -> (Bool, Bool) -> Chan J.JobMessage -> IO ()
        go _ (True, True) _ = return ()
        go prevJobMsg (isWebAppDone, isServerDone) chan = do
          jobMsg <- readChan chan
          case J._data jobMsg of
            J.JobOutput {} ->
              printPrefixedJobMessage prevJobMsg jobMsg
                >> go (Just jobMsg) (isWebAppDone, isServerDone) chan
            J.JobExit {} -> case J._jobType jobMsg of
              J.WebApp -> go (Just jobMsg) (True, isServerDone) chan
              J.Server -> go (Just jobMsg) (isWebAppDone, True) chan
              J.Db -> error "This should never happen. No db job should be active."

    setupFailedMessage (serverExitCode, webAppExitCode) =
      let serverErrorMessage = case serverExitCode of
            ExitFailure code -> " Server setup failed with exit code " ++ show code ++ "."
            _ -> ""
          webAppErrorMessage = case webAppExitCode of
            ExitFailure code -> " Web app setup failed with exit code " ++ show code ++ "."
            _ -> ""
       in "Setup failed!" ++ serverErrorMessage ++ webAppErrorMessage
