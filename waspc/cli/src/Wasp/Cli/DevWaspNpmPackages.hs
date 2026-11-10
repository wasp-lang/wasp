module Wasp.Cli.DevWaspNpmPackages
  ( DevWaspNpmPackageTarballs,
    devWaspNpmPackagesDirEnvVarName,
    devWaspNpmPackagesManifestFile,
    findDevWaspNpmPackageTarballs,
    devWaspNpmPackageSpecifier,
    checkDirectlyRunWaspUsesDevWaspNpmPackages,
    addDevWaspNpmPackageOverridesToPackageJsonFile,
    prepareDevWaspNpmPackagesForDockerBuild,
  )
where

import Control.Lens (at, non, (%~), (&), (.~), (?~), (^?))
import Control.Monad (unless)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Object, _String)
import Data.List (find, foldl', isSuffixOf, stripPrefix)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, File', Path', Rel, basename, fromAbsFile, fromRelFile, parseAbsDir, parseAbsFile, parseRelFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute)
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.WaspLibs.Common (LibsRootDir, libsRootDirInGeneratedAppDir)
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.WaspNpmPackage (isRunByWaspCliNpmPackage)
import Wasp.Util.IO (copyFile)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.InstallMethod (getGlobalInstallationCommand)
import Wasp.Util.Json (readJsonFile, updateJsonFile)

{-
Lets the Wasp apps in this repo run against the Wasp in this repo, before its
npm packages are published. Only the dev Wasp CLI (@waspc/tools/wasp-cli-dev@
and the dev \@wasp/cli, see @waspc/tools/devCliSubPackage.js@) sets
'devWaspNpmPackagesDirEnvVarName', so this does nothing for Wasp users.

`mise run build:dev-packages` packs the packages into `waspc/.dev-packages/`
(see @tasks/build/dev-packages.ts@) and lists the tarballs in
'devWaspNpmPackagesManifestFile' there. The repo's apps depend on the
tarballs. The dev @wasp/wasp still pins the registry versions of the packages
it brings, so `overrides` point npm at their tarballs too:

> "dependencies": {
>   "@wasp/wasp": "file:../../waspc/.dev-packages/wasp-wasp-0.27.0.tgz"
> },
> "overrides": {
>   "@wasp/cli": "file:../../waspc/.dev-packages/wasp-cli-0.27.0.tgz",
>   ...
> }

With the env var set, `wasp new` writes the tarballs into the new project, and
`wasp build` copies them into Docker's build context.

TODO: Remove this module once the repo's Wasp apps join the pnpm workspace.
-}

data DevWaspNpmPackageTarballs = DevWaspNpmPackageTarballs
  { waspTarball :: Path' Abs File',
    -- | By package name, for each package \@wasp/wasp brings.
    dependencyTarballs :: [(PJ.PackageName, Path' Abs File')]
  }

devWaspNpmPackagesDirEnvVarName :: String
devWaspNpmPackagesDirEnvVarName = "WASP_DEV_PACKAGES_DIR"

-- | A Wasp binary that \@wasp/cli doesn't run is a development build. Its
-- projects can only depend on the dev Wasp npm packages, so it needs
-- 'devWaspNpmPackagesDirEnvVarName'.
checkDirectlyRunWaspUsesDevWaspNpmPackages :: IO (Either String ())
checkDirectlyRunWaspUsesDevWaspNpmPackages = do
  isRunByNpmPackage <- isRunByWaspCliNpmPackage
  isDevPackagesDirSet <- isDevWaspNpmPackagesDirSet
  return $
    if isRunByNpmPackage || isDevPackagesDirSet
      then Right ()
      else
        Left $
          unlines
            [ "This Wasp binary was run directly, not through the `" ++ waspCliNpmPackageName ++ "` npm package.",
              "If you're developing Wasp, run it with `mise run wasp-cli`, or set " ++ devWaspNpmPackagesDirEnvVarName ++ " (`mise run install` prints the line to add to your shell profile).",
              "Otherwise, install Wasp with `" ++ getGlobalInstallationCommand Nothing ++ "`."
            ]

isDevWaspNpmPackagesDirSet :: IO Bool
isDevWaspNpmPackagesDirSet = isJust <$> lookupEnv devWaspNpmPackagesDirEnvVarName

-- | Written by `mise run build:dev-packages` into the dev packages dir. Names
-- the tarball of \@wasp/wasp and of each package it brings.
devWaspNpmPackagesManifestFile :: Path' (Rel d) File'
devWaspNpmPackagesManifestFile = [relfile|dev-packages.json|]

-- | 'Nothing' when 'devWaspNpmPackagesDirEnvVarName' isn't set. Fails when it's
-- set but the dir has no valid 'devWaspNpmPackagesManifestFile'.
findDevWaspNpmPackageTarballs :: IO (Maybe DevWaspNpmPackageTarballs)
findDevWaspNpmPackageTarballs =
  lookupEnv devWaspNpmPackagesDirEnvVarName >>= \case
    Nothing -> return Nothing
    Just devPackagesDirPath -> do
      devPackagesDir <- parseAbsDir devPackagesDirPath
      let manifestFile = devPackagesDir </> devWaspNpmPackagesManifestFile
      doesManifestExist <- IOUtil.doesFileExist manifestFile
      unless doesManifestExist $ failWith $ fromAbsFile manifestFile ++ " does not exist. Run `mise run build:dev-packages`."
      manifest <- readJsonFile manifestFile
      case manifest >>= readTarballsFromManifest devPackagesDir of
        Nothing -> failWith $ fromAbsFile manifestFile ++ " is not valid. Run `mise run build:dev-packages`."
        Just tarballs -> return $ Just tarballs
  where
    failWith :: String -> IO a
    failWith reason = ioError $ userError $ devWaspNpmPackagesDirEnvVarName ++ " is set, but " ++ reason

    readTarballsFromManifest :: Path' Abs (Dir d) -> Value -> Maybe DevWaspNpmPackageTarballs
    readTarballsFromManifest devPackagesDir manifest = do
      waspTarballFile <- manifest ^? key "waspTarball" . _String
      dependencyTarballFiles <- manifest ^? key "dependencyTarballs" . _Object
      DevWaspNpmPackageTarballs
        <$> tarballIn devPackagesDir waspTarballFile
        <*> mapM
          (\(name, tarballFile) -> (,) (Key.toString name) <$> (tarballFile ^? _String >>= tarballIn devPackagesDir))
          (KM.toList dependencyTarballFiles)

    tarballIn :: Path' Abs (Dir d) -> T.Text -> Maybe (Path' Abs File')
    tarballIn devPackagesDir tarballFile = (devPackagesDir </>) <$> parseRelFile (T.unpack tarballFile)

-- | What a new project's package.json says for \@wasp/wasp.
devWaspNpmPackageSpecifier :: DevWaspNpmPackageTarballs -> String
devWaspNpmPackageSpecifier = absTarballFileSpecifier . waspTarball

-- | Points npm at the dev tarballs of the packages \@wasp/wasp brings, and keeps
-- the `overrides` the package.json already has.
addDevWaspNpmPackageOverridesToPackageJsonFile :: DevWaspNpmPackageTarballs -> Path' Abs (File f) -> IO ()
addDevWaspNpmPackageOverridesToPackageJsonFile tarballs packageJsonFile =
  updateJsonFile addOverrides packageJsonFile >>= either (ioError . userError) return
  where
    addOverrides :: Value -> Value
    addOverrides packageJson = foldl' addOverride packageJson overrides

    addOverride :: Value -> (PJ.PackageName, String) -> Value
    addOverride packageJson (packageName, specifier) =
      packageJson
        & _Object . at "overrides" . non (object []) . _Object . at (Key.fromString packageName)
          ?~ String (T.pack specifier)

    overrides :: [(PJ.PackageName, String)]
    overrides = [(name, absTarballFileSpecifier tarball) | (name, tarball) <- dependencyTarballs tarballs]

-- | A package.json field that can point at a dev tarball, e.g.
-- `dependencies.@wasp/wasp` or `overrides.@wasp/cli`.
data DevTarballField = DevTarballField
  { section :: String,
    packageName :: PJ.PackageName
  }

devTarballFields :: DevWaspNpmPackageTarballs -> [DevTarballField]
devTarballFields tarballs =
  DevTarballField "dependencies" waspNpmPackageName
    : map (DevTarballField "overrides" . fst) (dependencyTarballs tarballs)

data DevTarballsDir

-- | Where `wasp build` copies the dev tarballs, next to the Wasp lib tarballs.
devTarballsDirInGeneratedAppDir :: Path' (Rel GeneratedAppDir) (Dir DevTarballsDir)
devTarballsDirInGeneratedAppDir = libsRootDirInGeneratedAppDir </> devTarballsDirInLibsRootDir
  where
    devTarballsDirInLibsRootDir :: Path' (Rel LibsRootDir) (Dir DevTarballsDir)
    devTarballsDirInLibsRootDir = [reldir|wasp|]

-- | Docker's build context is `.wasp/out/`, so we copy the tarballs into it and
-- point the build's package.json and lock at the copies. The copies have the
-- same bytes, so `npm ci` still finds the lock in sync.
--
-- Does nothing when 'devWaspNpmPackagesDirEnvVarName' isn't set.
prepareDevWaspNpmPackagesForDockerBuild ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratedAppDir) ->
  Path' Abs (File packageJson) ->
  Path' Abs (File packageLockJson) ->
  IO (Either String ())
prepareDevWaspNpmPackagesForDockerBuild waspProjectDir buildDir packageJsonInBuildDir packageLockJsonInBuildDir =
  findDevWaspNpmPackageTarballs >>= maybe (return $ Right ()) copyDevTarballsIntoBuildDir
  where
    copyDevTarballsIntoBuildDir :: DevWaspNpmPackageTarballs -> IO (Either String ())
    copyDevTarballsIntoBuildDir devTarballs = runExceptT $ do
      packageJson <- maybe (throwError "Couldn't read package.json in the build dir.") return =<< liftIO (readJsonFile packageJsonInBuildDir)
      let fieldsWithTarballs =
            mapMaybe
              (\field -> (,) field <$> getTarballPath (getField field packageJson))
              (devTarballFields devTarballs)
      unless (null fieldsWithTarballs) $ do
        let tarballsDirInBuildDir = buildDir </> devTarballsDirInGeneratedAppDir
        liftIO $ SD.createDirectoryIfMissing True $ SP.fromAbsDir tarballsDirInBuildDir
        copiedTarballs <- liftIO $ mapM (copyTarball tarballsDirInBuildDir) fieldsWithTarballs
        ExceptT $ updateJsonFile (\json -> foldl' pointFieldAtCopiedTarball json copiedTarballs) packageJsonInBuildDir
        ExceptT $ updateJsonFile (pointLockAtCopiedTarballs (map snd copiedTarballs)) packageLockJsonInBuildDir

    getField :: DevTarballField -> Value -> Maybe T.Text
    getField field json =
      json ^? key (Key.fromString $ section field) . key (Key.fromString $ packageName field) . _String

    setField :: DevTarballField -> T.Text -> Value -> Value
    setField field specifier json =
      json & key (Key.fromString $ section field) . key (Key.fromString $ packageName field) . _String .~ specifier

    getTarballPath :: Maybe T.Text -> Maybe FilePath
    getTarballPath specifier = specifier >>= stripPrefix "file:" . T.unpack

    copyTarball :: Path' Abs (Dir d) -> (DevTarballField, FilePath) -> IO (DevTarballField, Path' Abs File')
    copyTarball tarballsDirInBuildDir (field, tarballPath) = do
      srcTarball <-
        if isAbsolute tarballPath
          then parseAbsFile tarballPath
          else (waspProjectDir </>) <$> parseRelFile tarballPath
      let dstTarball = tarballsDirInBuildDir </> basename srcTarball
      copyFile srcTarball dstTarball
      return (field, dstTarball)

    pointFieldAtCopiedTarball :: Value -> (DevTarballField, Path' Abs File') -> Value
    pointFieldAtCopiedTarball json (field, tarball) =
      setField field (T.pack $ tarballSpecifierInDocker tarball) json

    -- The build's package.json lives at `/app` in Docker and the generated
    -- app at `/app/.wasp/out`, like in the Wasp project.
    tarballSpecifierInDocker :: Path' Abs File' -> String
    tarballSpecifierInDocker tarball =
      tarballFileSpecifier $ fromRelFile $ generatedAppDirInWaspProjectDir </> devTarballsDirInGeneratedAppDir </> basename tarball

    -- The root dependency on @wasp/wasp and the `resolved` field of each
    -- package entry.
    pointLockAtCopiedTarballs :: [Path' Abs File'] -> Value -> Value
    pointLockAtCopiedTarballs tarballs lock =
      lock
        & key "packages" . key "" . key "dependencies" . key (Key.fromString waspNpmPackageName) . _String %~ rewriteSpecifier
        & key "packages" . _Object %~ KM.map (key "resolved" . _String %~ rewriteSpecifier)
      where
        rewriteSpecifier specifier =
          maybe specifier (T.pack . tarballSpecifierInDocker) $ find (pointsAt specifier) tarballs

        pointsAt specifier tarball =
          "file:" `T.isPrefixOf` specifier
            && ("/" ++ fromRelFile (basename tarball)) `isSuffixOf` T.unpack specifier

absTarballFileSpecifier :: Path' Abs File' -> String
absTarballFileSpecifier = tarballFileSpecifier . fromAbsFile

-- | An npm `file:` specifier for a tarball. npm wants forward slashes even on
-- Windows, and a backslash would also be an invalid escape once written into JSON.
tarballFileSpecifier :: FilePath -> String
tarballFileSpecifier tarballPath = "file:" ++ map toForwardSlash tarballPath
  where
    toForwardSlash '\\' = '/'
    toForwardSlash c = c
