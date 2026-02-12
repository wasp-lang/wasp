module Wasp.Cli.Command.Build.DockerBuildContext
  ( prepareFilesNecessaryForDockerBuild,
    isWaspConfigPackageLocation,
  )
where

import Control.Exception (SomeException, catch, displayException)
import Control.Lens
import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Data.Aeson (Value)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens
import Data.List (isSuffixOf)
import StrongPath (Abs, Dir, Path', castRel, (</>))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Project.Common
  ( WaspProjectDir,
    getSrcTsConfigInWaspProjectDir,
    packageJsonInWaspProjectDir,
    packageLockJsonInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.WaspFile (findWaspFile)
import Wasp.Util.IO (copyDirectory, copyFile)
import Wasp.Util.Json (updateJsonFile)

prepareFilesNecessaryForDockerBuild ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir ProjectRootDir) ->
  IO (Either String ())
prepareFilesNecessaryForDockerBuild waspProjectDir buildDir = runExceptT $ do
  waspFilePath <- ExceptT $ findWaspFile waspProjectDir
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
  liftIOWithHandledExceptions $
    copyDirectory
      (waspProjectDir </> srcDirInWaspProjectDir)
      (buildDir </> castRel srcDirInWaspProjectDir)

  let packageJsonInBuildDir = buildDir </> castRel packageJsonInWaspProjectDir
  let packageLockJsonInBuildDir = buildDir </> castRel packageLockJsonInWaspProjectDir
  let tsconfigJsonInBuildDir = buildDir </> castRel srcTsConfigPath

  liftIOWithHandledExceptions $
    copyFile
      (waspProjectDir </> packageJsonInWaspProjectDir)
      packageJsonInBuildDir

  liftIOWithHandledExceptions $
    copyFile
      (waspProjectDir </> packageLockJsonInWaspProjectDir)
      packageLockJsonInBuildDir

  -- We need the main tsconfig.json file since the built server's TS config
  -- extends from it.
  liftIOWithHandledExceptions $
    copyFile
      (waspProjectDir </> srcTsConfigPath)
      tsconfigJsonInBuildDir

  -- A hacky quick fix for https://github.com/wasp-lang/wasp/issues/2368
  -- We should remove this code once we implement a proper solution.
  updatePackageJsonResult <-
    liftIOWithHandledExceptions $
      updateJsonFile removeWaspConfigFromDevDependenciesArray packageJsonInBuildDir
  ExceptT $ pure updatePackageJsonResult

  updatePackageLockJsonResult <-
    liftIOWithHandledExceptions $
      updateJsonFile removeAllMentionsOfWaspConfigInPackageLockJson packageLockJsonInBuildDir
  ExceptT $ pure updatePackageLockJsonResult
  where
    liftIOWithHandledExceptions :: IO a -> ExceptT String IO a
    liftIOWithHandledExceptions action =
      ExceptT $
        (Right <$> action)
          `catch` \(err :: SomeException) -> pure $ Left $ displayException err

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
  normalizedPackageLocation == "wasp-config" || "/wasp-config" `isSuffixOf` normalizedPackageLocation
  where
    normalizedPackageLocation = map normalizePathSeparator packageLocation
    normalizePathSeparator '\\' = '/'
    normalizePathSeparator c = c

removeWaspConfigFromDevDependenciesArray :: Value -> Value
removeWaspConfigFromDevDependenciesArray original =
  original & key "devDependencies" . _Object . at "wasp-config" .~ Nothing
