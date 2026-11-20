module Wasp.Generator.NpmInstall
  ( installNpmDependenciesWithInstallRecord,
    installProjectNpmDependencies,
    InstallOutputMode (..),
  )
where

import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec (waspProjectDir))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Monad (GeneratorError (..))
import Wasp.Generator.NpmInstall.Common (AllNpmDeps (..), getAllNpmDeps)
import Wasp.Generator.NpmInstall.InstalledNpmDepsLog (forgetInstalledNpmDepsLog, loadInstalledNpmDepsLog, saveInstalledNpmDepsLog)
import qualified Wasp.Job as Job
import qualified Wasp.Message as Msg
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util (checkExitSuccess)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.TaskStatus (runTaskWithStatusReport)

-- Installs the npm deps of the user's project and its workspaces (the generated server and the
-- Wasp SDK). Skips the install if `node_modules` exists and the installed npm deps log matches
-- the current deps. Saves the log after a successful install.
installNpmDependenciesWithInstallRecord ::
  AppSpec ->
  Path' Abs (Dir GeneratedAppDir) ->
  Msg.SendMessage ->
  IO (Either GeneratorError ())
installNpmDependenciesWithInstallRecord spec dstDir sendMessage = runExceptT $ do
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

    liftIO (installProjectNpmDependencies (HideInstallOutput sendMessage) waspProjectDirPath)
      >>= onLeftThrowError

    liftIO $ saveInstalledNpmDepsLog allNpmDeps dstDir
  where
    onLeftThrowError = either (throwError . GenericGeneratorError) pure

    waspProjectDirPath = waspProjectDir spec

-- | Whether npm's own output is shown to the user as it comes, or held back and
-- reported only if the installation fails.
data InstallOutputMode = ShowInstallOutput | HideInstallOutput Msg.SendMessage

{- HLINT ignore installProjectNpmDependencies "Avoid npm and npx" -}

-- Runs `npm install` in the user's Wasp project directory, which also installs the deps of all of
-- its npm workspaces (the generated server and the Wasp SDK). Depending on the output mode, npm's
-- own output is either printed as it comes, like when running npm by hand, or held back behind a
-- status line and included in the error if the installation fails.
installProjectNpmDependencies ::
  InstallOutputMode -> SP.Path SP.System Abs (Dir WaspProjectDir) -> IO (Either String ())
installProjectNpmDependencies installOutputMode projectDir = case installOutputMode of
  ShowInstallOutput -> checkExitSuccess "npm install" <$> Job.run npmInstall
  HideInstallOutput sendMessage ->
    runTaskWithStatusReport sendMessage "Installing dependencies..." "Dependencies installed" "Dependencies failed to install" $ do
      (exitCode, npmOutput) <- Job.run $ Job.captureOutput npmInstall
      return $ case checkExitSuccess "npm install" exitCode of
        Left errorMessage -> Left $ errorMessage ++ "\n\nnpm output:\n" ++ T.unpack npmOutput
        Right () -> Right ()
  where
    npmInstall = Job.fromProc $ Job.setCwd projectDir $ Job.proc "npm" ["install"]

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
