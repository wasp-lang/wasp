{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', castRel, fromRelDir, (</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspNpmPackageAvailable (WaspNpmPackageAvailable (WaspNpmPackageAvailable))
import Wasp.Cli.DevWaspNpmPackages (prepareDevWaspNpmPackagesForDockerBuild)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Setup (allSetupGoals)
import qualified Wasp.Message as Msg
import Wasp.Project (compileResultWarningsAndErrors)
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspProjectDir,
    generatedAppDirInWaspProjectDir,
    packageLockJsonInWaspProjectDir,
    srcDirInWaspProjectDir,
    srcTsConfig,
    tsConfigPaths,
    userPackageJsonInWaspProjectDir,
  )
import Wasp.Util.IO (copyDirectory, copyFile, doesDirectoryExist, removeDirectory)

-- | Builds Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp/out dir.
-- At the end, prints a report on how building went (by printing warnings, errors,
-- success/failure message, further steps, ...).
-- Finally, throws if there was a compile/build error.
-- Very similar to 'compile'.
build :: Command ()
build = ProjectLock.acquireExclusive $ do
  InWaspProject waspProjectDir <- require
  WaspNpmPackageAvailable <- require
  ValidNodeAndNpm <- require

  let buildDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDir
  when doesBuildDirExist $ do
    cliSendMessageC
      $ Msg.Start
      $ "Clearing the content of the " ++ fromRelDir generatedAppDirInWaspProjectDir ++ " directory..."
    liftIO $ removeDirectory buildDir
    cliSendMessageC
      $ Msg.Success
      $ "Successfully cleared the contents of the " ++ fromRelDir generatedAppDirInWaspProjectDir ++ " directory."

  cliSendMessageC $ Msg.Start "Building wasp project..."

  (warnings, errors) <- liftIO $ buildIO waspProjectDir buildDir
  liftIO $ printCompilationResult (warnings, errors)
  unless (null errors)
    $ throwError
    $ CommandError "Building of wasp project failed"
    $ show (length errors) ++ " errors found."

  liftIO (prepareFilesNecessaryForDockerBuild waspProjectDir buildDir) >>= \case
    Left err -> throwError $ CommandError "Failed to prepare files necessary for docker build" err
    Right () -> return ()

  cliSendMessageC
    $ Msg.Success
    $ "Your wasp project has been successfully built! Check it out in the " ++ fromRelDir generatedAppDirInWaspProjectDir ++ " directory."
  where
    prepareFilesNecessaryForDockerBuild waspProjectDir buildDir = runExceptT $ do
      let srcTsConfigPath = srcTsConfig tsConfigPaths

      -- Until we implement the solution described in https://github.com/wasp-lang/wasp/issues/1769,
      -- we're copying all files and folders necessary for Docker build into the .wasp/out directory.
      -- We chose this approach for 0.12.0 (instead of building from the project root) because:
      --   - The Docker build context remains small (~1.5 MB vs ~900 MB).
      --   - We don't risk copying possible secrets from the project root into Docker's build context.
      --   - The commands for building the project stay the same as before
      --     0.12.0, which is good for both us (e.g., for fly deployment) and our
      --     users  (no changes in CI/CD scripts).
      -- For more details, read the issue linked above.
      liftIO $
        copyDirectory
          (waspProjectDir </> srcDirInWaspProjectDir)
          (buildDir </> castRel srcDirInWaspProjectDir)

      let packageJsonInBuildDir = buildDir </> castRel userPackageJsonInWaspProjectDir
      let packageLockJsonInBuildDir = buildDir </> castRel packageLockJsonInWaspProjectDir
      let tsconfigJsonInBuildDir = buildDir </> castRel srcTsConfigPath

      liftIO $
        copyFile
          (waspProjectDir </> userPackageJsonInWaspProjectDir)
          packageJsonInBuildDir

      liftIO $
        copyFile
          (waspProjectDir </> packageLockJsonInWaspProjectDir)
          packageLockJsonInBuildDir

      -- We need the main tsconfig.json file since the built server's TS config
      -- extends from it.
      liftIO $
        copyFile
          (waspProjectDir </> srcTsConfigPath)
          tsconfigJsonInBuildDir

      ExceptT $ prepareDevWaspNpmPackagesForDockerBuild waspProjectDir buildDir packageJsonInBuildDir packageLockJsonInBuildDir

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir = compileResultWarningsAndErrors <$> compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { waspProjectDir,
          buildType = BuildType.Production,
          sendMessage = cliSendMessage,
          setupGoals = allSetupGoals
        }
