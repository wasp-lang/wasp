module Wasp.Cli.DevWaspNpmPackages
  ( DevWaspNpmPackageTarballs,
    devWaspNpmPackagesDirEnvVarName,
    findDevWaspNpmPackageTarballs,
    getNewProjectWaspNpmPackageSpecifier,
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
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, File', Path', Rel, basename, fromAbsFile, fromRelDir, fromRelFile, parseAbsDir, parseAbsFile, parseRelFile, reldir, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute)
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.WaspLibs.Common (libsRootDirInGeneratedAppDir)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.WaspNpmPackage (waspCliNpmPackageName, waspNpmPackageName, waspSpecNpmPackageName)
import Wasp.Util.IO (copyFile)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Json (readJsonFile, updateJsonFile)
import Wasp.Version (waspVersion)

{-
This module lets the Wasp apps in the Wasp repo run against the Wasp in the
repo, before its npm packages are published.

A Wasp project depends on Wasp through the @wasp/wasp npm package (see
"Wasp.Project.WaspNpmPackage"). While we develop a Wasp version, npm can't
install it: the registry doesn't have @wasp/wasp at that version yet, nor the
@wasp/cli and @wasp/spec versions it pins.

So `./run build:dev-packages` packs the packages from the repo into tarballs
in `waspc/.dev-packages/` (see @waspc/tools/dev-packages/build.ts@), and the
repo's Wasp apps depend on those tarballs instead:

> "dependencies": {
>   "@wasp/wasp": "file:../../waspc/.dev-packages/wasp-wasp-0.27.0.tgz"
> },
> "overrides": {
>   "@wasp/cli": "file:../../waspc/.dev-packages/wasp-cli-0.27.0.tgz",
>   "@wasp/spec": "file:../../waspc/.dev-packages/wasp-spec-0.27.0.tgz"
> }

The dev @wasp/wasp still pins the registry versions of @wasp/cli and
@wasp/spec, so the `overrides` point npm at their tarballs too.

The dev Wasp CLI (@waspc/tools/wasp-cli-dev@) tells Wasp where the tarballs
are through the 'devWaspNpmPackagesDirEnvVarName' env var. When it's set,
`wasp new` writes the tarballs into the new project's package.json, and
`wasp build` copies them into Docker's build context. A released Wasp CLI
doesn't set it, so this module does nothing for Wasp users.

TODO: Remove this module once the repo's Wasp apps join the pnpm workspace and
no longer depend on dev tarballs.
-}

data DevWaspNpmPackageTarballs = DevWaspNpmPackageTarballs
  { waspTarball :: Path' Abs File',
    cliTarball :: Path' Abs File',
    specTarball :: Path' Abs File'
  }

devWaspNpmPackagesDirEnvVarName :: String
devWaspNpmPackagesDirEnvVarName = "WASP_DEV_PACKAGES_DIR"

-- | Finds the dev tarballs in the dir 'devWaspNpmPackagesDirEnvVarName' points
-- at. Returns 'Nothing' when the env var isn't set, and fails when it's set but
-- a tarball is missing.
findDevWaspNpmPackageTarballs :: IO (Maybe DevWaspNpmPackageTarballs)
findDevWaspNpmPackageTarballs =
  lookupEnv devWaspNpmPackagesDirEnvVarName >>= \case
    Nothing -> return Nothing
    Just devPackagesDirPath -> do
      devPackagesDir <- parseAbsDir devPackagesDirPath
      let tarballs =
            DevWaspNpmPackageTarballs
              { waspTarball = devPackagesDir </> tarballFile "wasp-wasp",
                cliTarball = devPackagesDir </> tarballFile "wasp-cli",
                specTarball = devPackagesDir </> tarballFile "wasp-spec"
              }
      forM_ [waspTarball, cliTarball, specTarball] $ \getTarball -> do
        let tarball = getTarball tarballs
        exists <- IOUtil.doesFileExist tarball
        unless exists
          $ ioError
          $ userError
          $ devWaspNpmPackagesDirEnvVarName
            ++ " is set, but "
            ++ fromAbsFile tarball
            ++ " does not exist. Run `./run build:dev-packages` in waspc/."
      return $ Just tarballs
  where
    -- Named the way `npm pack` and `pnpm pack` name tarballs.
    tarballFile :: String -> Path' (Rel d) File'
    tarballFile name = fromJust $ parseRelFile $ name ++ "-" ++ show waspVersion ++ ".tgz"

-- | What a new project's package.json says for \@wasp/wasp: the dev tarball if
-- there is one, otherwise this CLI's version.
getNewProjectWaspNpmPackageSpecifier :: Maybe DevWaspNpmPackageTarballs -> String
getNewProjectWaspNpmPackageSpecifier = \case
  Nothing -> show waspVersion
  Just tarballs -> absTarballFileSpecifier $ waspTarball tarballs

-- | Adds the `overrides` that point npm at the dev tarballs of the packages
-- \@wasp/wasp depends on. Keeps the `overrides` the package.json already has.
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
    overrides =
      [ (waspCliNpmPackageName, absTarballFileSpecifier $ cliTarball tarballs),
        (waspSpecNpmPackageName, absTarballFileSpecifier $ specTarball tarballs)
      ]

-- | A package.json field that can point at a dev tarball, e.g.
-- `dependencies.@wasp/wasp` or `overrides.@wasp/cli`.
data DevTarballField = DevTarballField
  { section :: String,
    packageName :: PJ.PackageName
  }

devTarballFields :: [DevTarballField]
devTarballFields =
  [ DevTarballField "dependencies" waspNpmPackageName,
    DevTarballField "overrides" waspCliNpmPackageName,
    DevTarballField "overrides" waspSpecNpmPackageName
  ]

-- | Makes the dev tarballs available to the Docker build of `wasp build`.
--
-- Docker's build context is `.wasp/out/`, and the tarballs live outside of it.
-- So we copy them next to the Wasp libs and point the build's package.json and
-- package-lock.json at the copies. The lock entries keep their integrity,
-- because the copies have the same bytes, so `npm ci` still finds the lock in
-- sync. A project that depends on a released Wasp has no tarballs, so this
-- does nothing.
prepareDevWaspNpmPackagesForDockerBuild ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratedAppDir) ->
  Path' Abs (File packageJson) ->
  Path' Abs (File packageLockJson) ->
  IO (Either String ())
prepareDevWaspNpmPackagesForDockerBuild waspProjectDir buildDir packageJsonInBuildDir packageLockJsonInBuildDir = runExceptT $ do
  packageJson <- maybe (throwError "Couldn't read package.json in the build dir.") return =<< liftIO (readJsonFile packageJsonInBuildDir)
  let fieldsWithTarballs =
        mapMaybe
          (\field -> (,) field <$> getTarballPath (getField field packageJson))
          devTarballFields
  unless (null fieldsWithTarballs) $ do
    let tarballsDirInBuildDir = buildDir </> libsRootDirInGeneratedAppDir </> [reldir|wasp|]
    liftIO $ SD.createDirectoryIfMissing True $ SP.fromAbsDir tarballsDirInBuildDir
    copiedTarballs <- liftIO $ mapM (copyTarball tarballsDirInBuildDir) fieldsWithTarballs
    ExceptT $ updateJsonFile (\json -> foldl' pointFieldAtCopiedTarball json copiedTarballs) packageJsonInBuildDir
    ExceptT $ updateJsonFile (pointLockAtCopiedTarballs (map snd copiedTarballs)) packageLockJsonInBuildDir
  where
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

    -- The build's package.json lives at `/app` in Docker and the libs at
    -- `/app/.wasp/out/libs`, so the copied tarballs are reachable through that path.
    tarballSpecifierInDocker :: Path' Abs File' -> String
    tarballSpecifierInDocker tarball =
      tarballFileSpecifier $ fromRelDir generatedAppDirInWaspProjectDir ++ fromRelDir libsRootDirInGeneratedAppDir ++ "wasp/" ++ fromRelFile (basename tarball)

    -- Rewrites every `file:` specifier in the lock that points at one of the
    -- copied tarballs: the root dependency on @wasp/wasp and the `resolved`
    -- field of the packages' entries.
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
