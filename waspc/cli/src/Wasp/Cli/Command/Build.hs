module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', castRel, fromRelDir, (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import qualified Wasp.Message as Msg
import qualified Wasp.Project.BuildType as BuildType
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    getSrcTsConfigInWaspProjectDir,
    packageJsonInWaspProjectDir,
    packageLockJsonInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Util.IO (copyDirectory, copyFile, doesDirectoryExist, removeDirectory)

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

  let buildDirInWaspProjectDir = dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir
      buildDir = waspProjectDir </> buildDirInWaspProjectDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDir
  when doesBuildDirExist $ do
    cliSendMessageC $
      Msg.Start $
        "Clearing the content of the " ++ fromRelDir buildDirInWaspProjectDir ++ " directory..."
    liftIO $ removeDirectory buildDir
    cliSendMessageC $
      Msg.Success $
        "Successfully cleared the contents of the " ++ fromRelDir buildDirInWaspProjectDir ++ " directory."

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
      "Your wasp project has been successfully built! Check it out in the " ++ fromRelDir buildDirInWaspProjectDir ++ " directory."
  where
    prepareFilesNecessaryForDockerBuild waspProjectDir buildDir = runExceptT $ do
      (_, waspFilePath) <- ExceptT $ findWaspFile waspProjectDir
      let srcTsConfigPath = getSrcTsConfigInWaspProjectDir waspFilePath

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

      let packageJsonInBuildDir = buildDir </> castRel packageJsonInWaspProjectDir
      let packageLockJsonInBuildDir = buildDir </> castRel packageLockJsonInWaspProjectDir
      let tsconfigJsonInBuildDir = buildDir </> castRel srcTsConfigPath

      liftIO $
        copyFile
          (waspProjectDir </> packageJsonInWaspProjectDir)
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

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir = compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { waspProjectDirPath = waspProjectDir,
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
