module Wasp.Cli.DevWaspNpmPackages
  ( DevWaspNpmPackageTarballs,
    devWaspNpmPackagesDirEnvVarName,
    findDevWaspNpmPackageTarballs,
    devWaspNpmPackageTarballFile,
    devWaspNpmPackageSpecifier,
    checkDirectlyRunWaspUsesDevWaspNpmPackages,
    addDevWaspNpmPackageOverridesToPackageJsonFile,
    prepareDevWaspNpmPackagesForDockerBuild,
  )
where

import Control.Lens (at, non, (%~), (&), (.~), (?~), (^?))
import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Object, _String)
import Data.List (find, foldl', isSuffixOf, stripPrefix)
import Data.Maybe (fromJust, isJust, mapMaybe)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, File', Path', Rel, basename, fromAbsFile, fromRelFile, parseAbsDir, parseAbsFile, parseRelFile, reldir, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute)
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.WaspLibs.Common (LibsRootDir, libsRootDirInGeneratedAppDir)
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageDependencyNames, waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.WaspNpmPackage (isRunByWaspCliNpmPackage)
import Wasp.Util.IO (copyFile)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.InstallMethod (getGlobalInstallationCommand)
import Wasp.Util.Json (readJsonFile, updateJsonFile)
import Wasp.Version (waspVersion)

{-
Lets the Wasp apps in this repo run against the Wasp in this repo, before its
npm packages are published. Only the dev Wasp CLI (@waspc/tools/wasp-cli-dev@
and the dev \@wasp/cli, see @waspc/tools/devCliSubPackage.js@) sets
'devWaspNpmPackagesDirEnvVarName', so this does nothing for Wasp users.

`mise run build:dev-packages` packs the packages into `waspc/.dev-packages/`
(see @tasks/build/dev-packages.ts@), and the repo's apps depend on the
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
    -- | By package name, for each of 'waspNpmPackageDependencyNames'.
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

-- | 'Nothing' when 'devWaspNpmPackagesDirEnvVarName' isn't set. Fails when it's
-- set but a tarball is missing.
findDevWaspNpmPackageTarballs :: IO (Maybe DevWaspNpmPackageTarballs)
findDevWaspNpmPackageTarballs =
  lookupEnv devWaspNpmPackagesDirEnvVarName >>= \case
    Nothing -> return Nothing
    Just devPackagesDirPath -> do
      devPackagesDir <- parseAbsDir devPackagesDirPath
      let tarballOf packageName = devPackagesDir </> devWaspNpmPackageTarballFile packageName
          tarballs =
            DevWaspNpmPackageTarballs
              { waspTarball = tarballOf waspNpmPackageName,
                dependencyTarballs = [(name, tarballOf name) | name <- waspNpmPackageDependencyNames]
              }
      forM_ (waspTarball tarballs : map snd (dependencyTarballs tarballs)) $ \tarball -> do
        exists <- IOUtil.doesFileExist tarball
        unless exists
          $ ioError
          $ userError
          $ devWaspNpmPackagesDirEnvVarName
            ++ " is set, but "
            ++ fromAbsFile tarball
            ++ " does not exist. Run `mise run build:dev-packages`."
      return $ Just tarballs

-- | Named the way `npm pack` and `pnpm pack` name tarballs, e.g.
-- `wasp-cli-0.27.0.tgz` for \@wasp/cli.
devWaspNpmPackageTarballFile :: PJ.PackageName -> Path' (Rel d) File'
devWaspNpmPackageTarballFile packageName =
  fromJust $ parseRelFile $ map slashToDash (dropWhile (== '@') packageName) ++ "-" ++ show waspVersion ++ ".tgz"
  where
    slashToDash '/' = '-'
    slashToDash c = c

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

devTarballFields :: [DevTarballField]
devTarballFields =
  DevTarballField "dependencies" waspNpmPackageName
    : map (DevTarballField "overrides") waspNpmPackageDependencyNames

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
prepareDevWaspNpmPackagesForDockerBuild waspProjectDir buildDir packageJsonInBuildDir packageLockJsonInBuildDir = do
  isDevPackagesDirSet <- isDevWaspNpmPackagesDirSet
  if isDevPackagesDirSet
    then copyDevTarballsIntoBuildDir
    else return $ Right ()
  where
    copyDevTarballsIntoBuildDir :: IO (Either String ())
    copyDevTarballsIntoBuildDir = runExceptT $ do
      packageJson <- maybe (throwError "Couldn't read package.json in the build dir.") return =<< liftIO (readJsonFile packageJsonInBuildDir)
      let fieldsWithTarballs =
            mapMaybe
              (\field -> (,) field <$> getTarballPath (getField field packageJson))
              devTarballFields
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
