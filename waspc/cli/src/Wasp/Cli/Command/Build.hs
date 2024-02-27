module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', castRel, (</>))
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import qualified Wasp.Generator
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedCodeDir, sdkRootDirInProjectRootDir)
import qualified Wasp.Message as Msg
import Wasp.Project (CompileError, CompileWarning, WaspProjectDir)
import Wasp.Project.Common (buildDirInDotWaspDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir, packageJsonInWaspProjectDir, packageLockJsonInWaspProjectDir, srcDirInWaspProjectDir)
import Wasp.Util.IO (copyDirectory, copyFile, doesDirectoryExist, removeDirectory)

-- | Builds Wasp project that the current working directory is part of.
-- Does all the steps, from analysis to generation, and at the end writes generated code
-- to the disk, to the .wasp/build dir.
-- At the end, prints a report on how building went (by printing warnings, errors,
-- success/failure message, further steps, ...).
-- Finally, throws if there was a compile/build error.
-- Very similar to 'compile'.
build :: Command ()
build = do
  InWaspProject waspProjectDir <- require
  let buildDir =
        waspProjectDir </> dotWaspDirInWaspProjectDir
          </> buildDirInDotWaspDir

  doesBuildDirExist <- liftIO $ doesDirectoryExist buildDir
  when doesBuildDirExist $ do
    cliSendMessageC $ Msg.Start "Clearing the content of the .wasp/build directory..."
    liftIO $ removeDirectory buildDir
    cliSendMessageC $ Msg.Success "Successfully cleared the contents of the .wasp/build directory."

  -- We are using the same SDK location for both build and start. Read this issue
  -- for the full story: https://github.com/wasp-lang/wasp/issues/1769
  let sdkDir = waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> sdkRootDirInProjectRootDir
  doesSdkDirExist <- liftIO $ doesDirectoryExist sdkDir
  when doesSdkDirExist $ do
    cliSendMessageC $ Msg.Start "Clearing the content of the .wasp/out/sdk directory..."
    liftIO $ removeDirectory sdkDir
    cliSendMessageC $ Msg.Success "Successfully cleared the contents of the .wasp/out/sdk directory."

  cliSendMessageC $ Msg.Start "Building wasp project..."

  (warnings, errors) <- liftIO $ buildIO waspProjectDir buildDir
  liftIO $ printCompilationResult (warnings, errors)
  unless (null errors) $
    throwError $
      CommandError "Building of wasp project failed" $ show (length errors) ++ " errors found."

  liftIO $ copyUserFilesNecessaryForBuild waspProjectDir buildDir

  cliSendMessageC $
    Msg.Success "Your wasp project has been successfully built! Check it out in the .wasp/build directory."
  where
    -- Until we implement the solution described in https://github.com/wasp-lang/wasp/issues/1769,
    -- we're copying all files and folders necessary for the build into the .wasp/build directory.
    -- We chose this approach for 0.12.0 (instead of building from the project root) because:
    --   - The build context remains small (~1.5 MB vs ~900 MB).
    --   - We don't risk copying possible secrets from the project root into the build context.
    --   - The commands for building the project stay the same as before
    --     0.12.0, which is good for both us (e.g., for fly deployment) and our
    --     users  (no changes in CI/CD scripts).
    -- For more details, read the issue linked above.
    copyUserFilesNecessaryForBuild waspProjectDir buildDir = do
      copyDirectory
        (waspProjectDir </> srcDirInWaspProjectDir)
        (buildDir </> castRel srcDirInWaspProjectDir)

      copyDirectory
        (waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> sdkRootDirInGeneratedCodeDir)
        (buildDir </> sdkRootDirInGeneratedCodeDir)

      copyFile
        (waspProjectDir </> packageJsonInWaspProjectDir)
        (buildDir </> castRel packageJsonInWaspProjectDir)

      copyFile
        (waspProjectDir </> packageLockJsonInWaspProjectDir)
        (buildDir </> castRel packageLockJsonInWaspProjectDir)

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir Wasp.Generator.ProjectRootDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir = compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { waspProjectDirPath = waspProjectDir,
          isBuild = True,
          sendMessage = cliSendMessage,
          -- Ignore "DB needs migration warnings" during build, as that is not a required step.
          generatorWarningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }
