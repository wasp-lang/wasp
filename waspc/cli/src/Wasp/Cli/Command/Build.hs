{-# LANGUAGE NamedFieldPuns #-}

module Wasp.Cli.Command.Build
  ( build,
  )
where

import Control.Lens ((%~), (&), (.~), (^?))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Object, _String)
import Data.Either (fromLeft)
import Data.List (find, isSuffixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File', Path', basename, castRel, fromRelDir, fromRelFile, parseAbsFile, parseRelFile, reldir, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import System.FilePath (isAbsolute)
import Wasp.Cli.Command (Command, CommandError (..), require)
import Wasp.Cli.Command.Compile (compileIOWithOptions, printCompilationResult)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspNpmPackageAvailable (WaspNpmPackageAvailable (WaspNpmPackageAvailable))
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.CompileOptions (CompileOptions (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.Setup (allSetupGoals)
import Wasp.Generator.WaspLibs.Common (libsRootDirInGeneratedAppDir)
import qualified Wasp.Message as Msg
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
import Wasp.Project.WaspNpmPackage (tarballFileSpecifier, waspCliNpmPackageName, waspNpmPackageName, waspSpecNpmPackageName)
import Wasp.Util.IO (copyDirectory, copyFile, doesDirectoryExist, removeDirectory)
import Wasp.Util.Json (readJsonFile, updateJsonFile)

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

      ExceptT $ prepareDevWaspNpmPackageForDockerBuild waspProjectDir buildDir packageJsonInBuildDir packageLockJsonInBuildDir

    -- While developing Wasp, the project depends on dev tarballs of @wasp/wasp,
    -- @wasp/cli and @wasp/spec outside the project (see "Wasp.Project.WaspNpmPackage").
    -- Docker's build context is `.wasp/out/`, so we copy the tarballs next to
    -- the Wasp libs and point the build's package.json and package-lock.json at
    -- the copies. The lock entries keep their integrity, since the tarballs are
    -- the same, so `npm ci` still finds the lock in sync.
    -- Released Wasp versions are registry dependencies, so this does nothing for them.
    --
    -- TODO: Remove once the repo's Wasp apps join the pnpm workspace and no
    -- longer depend on dev tarballs.
    prepareDevWaspNpmPackageForDockerBuild waspProjectDir buildDir packageJsonInBuildDir packageLockJsonInBuildDir = runExceptT $ do
      packageJson <- maybe (throwError "Couldn't read package.json in the build dir.") return =<< liftIO (readJsonFile packageJsonInBuildDir)
      -- (package.json section, package name) of every field that can hold a dev tarball.
      let specifierFields =
            [ ("dependencies", waspNpmPackageName),
              ("overrides", waspCliNpmPackageName),
              ("overrides", waspSpecNpmPackageName)
            ]
          devTarballSpecifiers =
            mapMaybe
              (\field -> withField field <$> getDevTarballSpecifier (getSpecifierField field packageJson))
              specifierFields
      unless (null devTarballSpecifiers) $ do
        let devPackagesDirInBuildDir = buildDir </> libsRootDirInGeneratedAppDir </> [reldir|wasp|]
        liftIO $ SD.createDirectoryIfMissing True $ SP.fromAbsDir devPackagesDirInBuildDir
        copiedTarballs <- liftIO $ mapM (copyDevTarball waspProjectDir devPackagesDirInBuildDir) devTarballSpecifiers
        ExceptT $ updateJsonFile (\json -> foldl setDevTarballSpecifierInDocker json copiedTarballs) packageJsonInBuildDir
        ExceptT $ updateJsonFile (rewriteDevTarballSpecifiersInLock (map snd copiedTarballs)) packageLockJsonInBuildDir

    withField :: field -> FilePath -> (field, FilePath)
    withField field tarballPath = (field, tarballPath)

    getSpecifierField :: (String, String) -> Value -> Maybe T.Text
    getSpecifierField (section, packageName) json =
      json ^? key (Key.fromString section) . key (Key.fromString packageName) . _String

    setSpecifierField :: (String, String) -> T.Text -> Value -> Value
    setSpecifierField (section, packageName) specifier json =
      json & key (Key.fromString section) . key (Key.fromString packageName) . _String .~ specifier

    getDevTarballSpecifier :: Maybe T.Text -> Maybe FilePath
    getDevTarballSpecifier specifier = specifier >>= stripPrefix "file:" . T.unpack

    copyDevTarball :: Path' Abs (Dir WaspProjectDir) -> Path' Abs (Dir d) -> (field, FilePath) -> IO (field, Path' Abs File')
    copyDevTarball waspProjectDir devPackagesDirInBuildDir (field, tarballPath) = do
      srcTarball <-
        if isAbsolute tarballPath
          then parseAbsFile tarballPath
          else (waspProjectDir </>) <$> parseRelFile tarballPath
      let dstTarball = devPackagesDirInBuildDir </> basename srcTarball
      copyFile srcTarball dstTarball
      return (field, dstTarball)

    setDevTarballSpecifierInDocker :: Value -> ((String, String), Path' Abs File') -> Value
    setDevTarballSpecifierInDocker json (field, tarball) =
      setSpecifierField field (T.pack $ devTarballSpecifierInDocker tarball) json

    -- The build's package.json lives at `/app` in Docker and the libs at
    -- `/app/.wasp/out/libs`, so the tarballs are reachable through that path.
    devTarballSpecifierInDocker :: Path' Abs File' -> String
    devTarballSpecifierInDocker tarball =
      tarballFileSpecifier $ fromRelDir generatedAppDirInWaspProjectDir ++ fromRelDir libsRootDirInGeneratedAppDir ++ "wasp/" ++ fromRelFile (basename tarball)

    -- Rewrites every `file:` specifier in the lock that points at one of the
    -- copied tarballs: the root dependency on @wasp/wasp and the `resolved`
    -- field of the packages' entries.
    rewriteDevTarballSpecifiersInLock :: [Path' Abs File'] -> Value -> Value
    rewriteDevTarballSpecifiersInLock tarballs lock =
      lock
        & key "packages" . key "" . key "dependencies" . key (Key.fromString waspNpmPackageName) . _String %~ rewriteSpecifier
        & key "packages" . _Object %~ KM.map (key "resolved" . _String %~ rewriteSpecifier)
      where
        rewriteSpecifier specifier =
          maybe specifier (T.pack . devTarballSpecifierInDocker) $ find (pointsAt specifier) tarballs

        pointsAt specifier tarball =
          "file:" `T.isPrefixOf` specifier
            && ("/" ++ fromRelFile (basename tarball)) `isSuffixOf` T.unpack specifier

buildIO ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO ([CompileWarning], [CompileError])
buildIO waspProjectDir buildDir =
  fmap (fromLeft [])
    <$> compileIOWithOptions options waspProjectDir buildDir
  where
    options =
      CompileOptions
        { waspProjectDir,
          buildType = BuildType.Production,
          sendMessage = cliSendMessage,
          setupGoals = allSetupGoals
        }
