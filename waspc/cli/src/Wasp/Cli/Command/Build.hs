module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Lens
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.List (isSuffixOf)
import StrongPath (Abs, Dir, Path', castRel, (</>))
import qualified System.FilePath as FP
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Message (cliSendMessage)
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedCodeDir, sdkRootDirInProjectRootDir)
import qualified Wasp.Message as Msg
import Wasp.Project.Common
  ( CompileError,
    CompileWarning,
    WaspProjectDir,
    buildDirInDotWaspDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    getSrcTsConfigInWaspProjectDir,
    packageJsonInWaspProjectDir,
    packageLockJsonInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Util.IO (copyDirectory, copyFile, doesDirectoryExist, removeDirectory)
import Wasp.Util.Json (updateJsonFile)

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
        waspProjectDir
          </> dotWaspDirInWaspProjectDir
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
      CommandError "Building of wasp project failed" $
        show (length errors) ++ " errors found."

  liftIO (prepareFilesNecessaryForDockerBuild waspProjectDir buildDir) >>= \case
    Left err -> throwError $ CommandError "Failed to prepare files necessary for docker build" err
    Right () -> return ()

  cliSendMessageC $
    Msg.Success "Your wasp project has been successfully built! Check it out in the .wasp/build directory."
  where
    prepareFilesNecessaryForDockerBuild waspProjectDir buildDir = runExceptT $ do
      waspFilePath <- ExceptT $ findWaspFile waspProjectDir
      let srcTsConfigPath = getSrcTsConfigInWaspProjectDir waspFilePath

      -- Until we implement the solution described in https://github.com/wasp-lang/wasp/issues/1769,
      -- we're copying all files and folders necessary for Docker build into the .wasp/build directory.
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

      liftIO $
        copyDirectory
          (waspProjectDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> sdkRootDirInGeneratedCodeDir)
          (buildDir </> sdkRootDirInGeneratedCodeDir)

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

      -- A hacky quick fix for https://github.com/wasp-lang/wasp/issues/2368
      -- We should remove this code once we implement a proper solution.
      ExceptT $ updateJsonFile removeWaspConfigFromDevDependenciesArray packageJsonInBuildDir
      ExceptT $ updateJsonFile removeAllMentionsOfWaspConfigInPackageLockJson packageLockJsonInBuildDir

    removeAllMentionsOfWaspConfigInPackageLockJson :: Value -> Value
    removeAllMentionsOfWaspConfigInPackageLockJson packageLockJsonObject =
      -- We want to:
      --   1. Remove the `wasp-config` dev dependency from the root package in package-lock.json.
      --   This is at `packageLock["packages"][""]["wasp-config"]`.
      --   2. Remove all package location entries for the `wasp-config` package
      --   (i.e., entries whose location keys end in `/wasp-config`).
      --   Example locations include:
      --      packageLock["packages"]["../../data/packages/wasp-config"]
      --      packageLock["packages"]["node_modules/wasp-config"]
      --      packageLock["packages"]["/home/filip/../wasp-config"]
      packageLockJsonObject
        & key "packages" . key "" %~ removeWaspConfigFromDevDependenciesArray
        & key "packages" . _Object
          %~ KM.filterWithKey
            (\packageLocation _ -> not $ isWaspConfigPackageLocation (Key.toString packageLocation))

    isWaspConfigPackageLocation :: String -> Bool
    isWaspConfigPackageLocation packageLocation =
      (FP.pathSeparator : "wasp-config") `isSuffixOf` packageLocation

    removeWaspConfigFromDevDependenciesArray :: Value -> Value
    removeWaspConfigFromDevDependenciesArray original =
      original & key "devDependencies" . _Object . at "wasp-config" .~ Nothing

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
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
