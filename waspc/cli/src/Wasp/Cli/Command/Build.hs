{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Lens (at, (%~), (&), (.~))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens (key, _Object)
import StrongPath (Abs, Dir, Path', castRel, fromRelDir, (</>))
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import qualified Wasp.Message as Msg
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), getInstallablePackageName)
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
import Wasp.Util.Json (updateJsonFile)

-- | Builds Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp/out dir.
-- At the end, prints a report on how building went (by printing warnings, errors,
-- success/failure message, further steps, ...).
-- Finally, throws if there was a compile/build error.
-- Very similar to 'compile'.
build :: Command ()
build = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  ValidNodeAndNpm <- require

  let buildDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDir
  when doesBuildDirExist $ do
    cliSendMessageC $
      Msg.Start $
        "Clearing the content of the " ++ fromRelDir generatedAppDirInWaspProjectDir ++ " directory..."
    liftIO $ removeDirectory buildDir
    cliSendMessageC $
      Msg.Success $
        "Successfully cleared the contents of the " ++ fromRelDir generatedAppDirInWaspProjectDir ++ " directory."

  cliSendMessageC $ Msg.Start "Building wasp project..."

  (warnings, errors) <- liftIO $ buildIO waspProjectDir buildDir
  liftIO $ printCompilationResult (warnings, errors)
  unless (null errors) $
    throwError $
      CommandError "Building of wasp project failed" $
        show (length errors) ++ " errors found."

  liftIO (prepareFilesNecessaryForDockerBuild waspProjectDir buildDir) >>= \case
    Left err -> throwError $ CommandError "Failed to prepare files necessary for docker build" err
    Right () -> return ()

  cliSendMessageC $
    Msg.Success $
      "Your wasp project has been successfully built! Check it out in the " ++ fromRelDir generatedAppDirInWaspProjectDir ++ " directory."
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

      -- The user's `package.json` references `@wasp.sh/spec` via `file:.wasp/spec`,
      -- but Docker's build context is `.wasp/out/` and doesn't include `.wasp/spec/`.
      -- We strip `@wasp.sh/spec` from the build's `devDependencies` so that Docker's
      -- `npm install` reconciles the lockfile (dropping the now-orphan spec entries)
      -- and proceeds without trying to resolve the missing `file:` path.
      --
      -- This relies on `npm install` (not `npm ci`) being used in the Dockerfile.
      -- The proper fix(es) are tracked in:
      --   - https://github.com/wasp-lang/wasp/issues/897
      --   - https://github.com/wasp-lang/wasp/issues/1769
      ExceptT $ updateJsonFile removeWaspSpecFromDevDependencies packageJsonInBuildDir
      -- Removing `@wasp.sh/spec` from the package-lock.json file is not
      -- strictly necessary (`npm install` would drop it anyway). We still do
      -- it because:
      --   - Npm throws a warning if it detects a mismatch.
      --   - Npm has become more strict about this over time and will possibly
      --   upgrade the warning into an error in future versions.
      ExceptT $ updateJsonFile (key "packages" . key "" %~ removeWaspSpecFromDevDependencies) packageLockJsonInBuildDir

    removeWaspSpecFromDevDependencies :: Value -> Value
    removeWaspSpecFromDevDependencies original =
      original & key "devDependencies" . _Object . at (Key.fromString waspSpecPackageName) .~ Nothing

    waspSpecPackageName :: String
    waspSpecPackageName = getInstallablePackageName WaspSpecPackage

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir = compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { waspProjectDir,
          buildType = BuildType.Production,
          sendMessage = cliSendMessage,
          -- Ignore "DB needs migration warnings" during build, as that is not a required step.
          generatorWarningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }
