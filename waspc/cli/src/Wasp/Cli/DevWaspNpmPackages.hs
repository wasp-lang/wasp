module Wasp.Cli.DevWaspNpmPackages
  ( DevWaspNpmPackageTarballs,
    devWaspNpmPackagesDirEnvVarName,
    devWaspNpmPackagesManifestFile,
    findDevWaspNpmPackageTarballs,
    devWaspNpmPackageSpecifier,
    checkDirectlyRunWaspCanCreateProjects,
    addDevWaspNpmPackageOverridesToPackageJsonFile,
    getDevWaspNpmPackageOverrides,
    prepareDevWaspNpmPackagesForDockerBuild,
  )
where

import Control.Lens (Traversal', at, non, (%~), (&), (.~), (?~), (^?))
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (String), object, parseJSON)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Lens (key, _Object, _String)
import Data.Aeson.Types (parseEither)
import Data.List (find, foldl', isSuffixOf, stripPrefix)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import StrongPath (Abs, Dir, File, File', Path', Rel, basename, fromAbsFile, fromRelFile, parseAbsDir, parseAbsFile, parseRelFile, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as SD
import System.Environment (lookupEnv)
import System.FilePath (isAbsolute)
import qualified Wasp.Cli.PnpmWorkspaceFile as PnpmWorkspaceFile
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DockerGenerator (devWaspNpmPackagesDirInGeneratedAppDir)
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import Wasp.Project.PackageManager (PackageManager (..), getLockfileName)
import Wasp.Project.WaspNpmPackage (dependsOnDevWaspNpmPackages)
import Wasp.Util.IO (copyFile)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.InstallMethod (getGlobalInstallationCommand)
import Wasp.Util.Json (readJsonFile, updateJsonFile)
import Wasp.Version (isRunByWaspCliNpmPackage)

{-
Lets Wasp projects run against the Wasp in this repo, before its npm packages
are published. Only the dev Wasp CLI (@waspc/tools/wasp-cli-dev@ and the dev
\@wasp/cli) sets 'devWaspNpmPackagesDirEnvVarName', and only projects that
develop Wasp depend on the unreleased packages, so this does nothing for Wasp
users.

The repo's own Wasp apps are part of its pnpm workspace and depend on
\@wasp/wasp through `workspace:*`. A project outside of the workspace, which
`wasp new` creates with the dev CLI, depends on tarballs instead.
`mise run build:dev-packages` packs the packages into `waspc/.dev-packages/`
(see @tasks/build/dev-packages.ts@) and lists the tarballs in
'devWaspNpmPackagesManifestFile' there. The dev \@wasp/wasp still pins the
registry versions of the packages it brings, so overrides point the package
manager at their tarballs too. For npm:

> "dependencies": {
>   "@wasp/wasp": "file:/repo/waspc/.dev-packages/wasp-wasp-0.27.0.tgz"
> },
> "overrides": {
>   "@wasp/cli": "file:/repo/waspc/.dev-packages/wasp-cli-0.27.0.tgz",
>   ...
> }

With the env var set, `wasp new` writes the tarballs into the new project. A
Docker image can't reach the workspace or the tarballs, so for both kinds of
projects `wasp build` copies the tarballs into Docker's build context.
-}

data DevWaspNpmPackageTarballs = DevWaspNpmPackageTarballs
  { waspTarball :: Path' Abs File',
    -- | By package name, for each package \@wasp/wasp brings.
    dependencyTarballs :: [(PJ.PackageName, Path' Abs File')]
  }

devWaspNpmPackagesDirEnvVarName :: String
devWaspNpmPackagesDirEnvVarName = "WASP_DEV_PACKAGES_DIR"

-- | A Wasp binary that \@wasp/cli doesn't run is a development build, whose
-- version isn't on npm. The projects it creates can only depend on the dev Wasp
-- npm packages, so `wasp new` needs 'devWaspNpmPackagesDirEnvVarName'.
checkDirectlyRunWaspCanCreateProjects :: IO (Either String ())
checkDirectlyRunWaspCanCreateProjects = do
  isRunByNpmPackage <- isRunByWaspCliNpmPackage
  isDevPackagesDirSet <- isJust <$> lookupEnv devWaspNpmPackagesDirEnvVarName
  return $
    if isRunByNpmPackage || isDevPackagesDirSet
      then Right ()
      else
        Left $
          unlines
            [ "This Wasp binary was run directly, not through the `" ++ waspCliNpmPackageName ++ "` npm package, so the projects it creates must depend on the dev Wasp npm packages.",
              "If you're developing Wasp, run `mise run wasp-cli new`, or set " ++ devWaspNpmPackagesDirEnvVarName ++ " (`mise run install` prints its value).",
              "Otherwise, install Wasp with `" ++ getGlobalInstallationCommand Nothing ++ "`."
            ]

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
    Just devPackagesDirPath ->
      parseAbsDir devPackagesDirPath
        >>= readDevWaspNpmPackageTarballs
        >>= either (ioError . userError . ((devWaspNpmPackagesDirEnvVarName ++ " is set, but ") ++)) (return . Just)

-- | Reads 'devWaspNpmPackagesManifestFile' of the dev packages dir.
readDevWaspNpmPackageTarballs :: Path' Abs (Dir d) -> IO (Either String DevWaspNpmPackageTarballs)
readDevWaspNpmPackageTarballs devPackagesDir = runExceptT $ do
  doesManifestExist <- liftIO $ IOUtil.doesFileExist manifestFile
  unless doesManifestExist $ throwError $ fromAbsFile manifestFile ++ " does not exist. Run `mise run build:dev-packages`."
  manifest <- liftIO $ readJsonFile manifestFile
  maybe (throwError $ fromAbsFile manifestFile ++ " is not valid. Run `mise run build:dev-packages`.") return $
    manifest >>= readTarballsFromManifest
  where
    manifestFile = devPackagesDir </> devWaspNpmPackagesManifestFile

    readTarballsFromManifest :: Value -> Maybe DevWaspNpmPackageTarballs
    readTarballsFromManifest manifest = do
      waspTarballFile <- manifest ^? key "waspTarball" . _String
      dependencyTarballFiles <- manifest ^? key "dependencyTarballs" . _Object
      DevWaspNpmPackageTarballs
        <$> tarballInDevPackagesDir waspTarballFile
        <*> mapM
          (\(name, tarballFile) -> (,) (Key.toString name) <$> (tarballFile ^? _String >>= tarballInDevPackagesDir))
          (KM.toList dependencyTarballFiles)

    tarballInDevPackagesDir :: T.Text -> Maybe (Path' Abs File')
    tarballInDevPackagesDir tarballFile = (devPackagesDir </>) <$> parseRelFile (T.unpack tarballFile)

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

    overrides = getDevWaspNpmPackageOverrides tarballs

-- | Each package that \@wasp/wasp brings, and the specifier of its dev tarball.
getDevWaspNpmPackageOverrides :: DevWaspNpmPackageTarballs -> [(PJ.PackageName, String)]
getDevWaspNpmPackageOverrides tarballs =
  [(name, absTarballFileSpecifier tarball) | (name, tarball) <- dependencyTarballs tarballs]

-- | Docker's build context is `.wasp/out/`, so we copy the tarballs into it,
-- where the Dockerfile picks them up, and point the build's package.json at
-- the copies, and the overrides of the packages that \@wasp/wasp brings.
--
-- Does nothing for a project that doesn't depend on the unreleased packages
-- (see 'dependsOnDevWaspNpmPackages'). Finds the tarballs next to the
-- project's \@wasp/wasp tarball. A project of the repo's pnpm workspace has
-- none, so it needs 'devWaspNpmPackagesDirEnvVarName'.
prepareDevWaspNpmPackagesForDockerBuild ::
  PackageManager ->
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs (Dir GeneratedAppDir) ->
  IO (Either String ())
prepareDevWaspNpmPackagesForDockerBuild packageManager waspProjectDir buildDir = runExceptT $ do
  packageJson <- maybe (throwError "Couldn't read package.json in the build dir.") return =<< liftIO (readJsonFile packageJsonInBuildDir)
  parsedPackageJson <- either (throwError . ("Couldn't read package.json in the build dir: " ++)) return $ parseEither parseJSON packageJson
  when (dependsOnDevWaspNpmPackages parsedPackageJson) $ do
    tarballs <- case packageJson ^? waspDependency >>= stripPrefix "file:" . T.unpack of
      Just waspTarballPath -> do
        projectWaspTarball <-
          liftIO $
            if isAbsolute waspTarballPath
              then parseAbsFile waspTarballPath
              else (waspProjectDir </>) <$> parseRelFile waspTarballPath
        ExceptT $ readDevWaspNpmPackageTarballs $ SP.parent projectWaspTarball
      -- A project of the Wasp repo's pnpm workspace.
      Nothing ->
        liftIO findDevWaspNpmPackageTarballs
          >>= maybe (throwError $ "The project depends on the Wasp of a pnpm workspace, which only works in the Wasp repo, with " ++ devWaspNpmPackagesDirEnvVarName ++ " set.") return

    liftIO $ SD.createDirectoryIfMissing True $ SP.fromAbsDir tarballsDirInBuildDir
    liftIO $ mapM_ copyTarballIntoBuildDir $ waspTarball tarballs : map snd (dependencyTarballs tarballs)

    let overrides = [(name, tarballSpecifierInDocker tarball) | (name, tarball) <- dependencyTarballs tarballs]
    ExceptT $ updateJsonFile (waspDependency .~ T.pack (tarballSpecifierInDocker $ waspTarball tarballs)) packageJsonInBuildDir
    case packageManager of
      Npm -> do
        ExceptT $ updateJsonFile (setOverridesInPackageJson overrides) packageJsonInBuildDir
        -- The copies have the same bytes, so `npm ci` still finds the lock in sync.
        ExceptT $ updateJsonFile (pointNpmLockAtCopiedTarballs tarballs) (lockfileInBuildDir Npm)
      Bun -> do
        ExceptT $ updateJsonFile (setOverridesInPackageJson overrides) packageJsonInBuildDir
        liftIO $ IOUtil.deleteFileIfExists $ lockfileInBuildDir Bun
      Pnpm -> do
        -- pnpm only reads overrides from its workspace file.
        liftIO $ do
          doesWorkspaceFileExist <- IOUtil.doesFileExist pnpmWorkspaceFileInBuildDir
          settings <- if doesWorkspaceFileExist then T.unpack <$> IOUtil.readFileStrict pnpmWorkspaceFileInBuildDir else return ""
          IOUtil.writeFile pnpmWorkspaceFileInBuildDir
            $ PnpmWorkspaceFile.appendOverrides overrides
            $ PnpmWorkspaceFile.removeTopLevelKey "overrides" settings
        liftIO $ IOUtil.deleteFileIfExists $ lockfileInBuildDir Pnpm
  where
    packageJsonInBuildDir :: Path' Abs File'
    packageJsonInBuildDir = buildDir </> [relfile|package.json|]

    pnpmWorkspaceFileInBuildDir :: Path' Abs File'
    pnpmWorkspaceFileInBuildDir = buildDir </> [relfile|pnpm-workspace.yaml|]

    -- The lockfile points at the tarballs outside the build context. We only
    -- know how to point npm's at the copies, so the others install without one.
    lockfileInBuildDir :: PackageManager -> Path' Abs File'
    lockfileInBuildDir = (buildDir </>) . fromJust . parseRelFile . getLockfileName

    tarballsDirInBuildDir = buildDir </> devWaspNpmPackagesDirInGeneratedAppDir

    waspDependency :: Traversal' Value T.Text
    waspDependency = key "dependencies" . key (Key.fromString waspNpmPackageName) . _String

    copyTarballIntoBuildDir :: Path' Abs File' -> IO ()
    copyTarballIntoBuildDir tarball = copyFile tarball (tarballsDirInBuildDir </> basename tarball)

    setOverridesInPackageJson :: [(PJ.PackageName, String)] -> Value -> Value
    setOverridesInPackageJson overrides packageJson = foldl' setOverride packageJson overrides
      where
        setOverride json (packageName, specifier) =
          json & _Object . at "overrides" . non (object []) . _Object . at (Key.fromString packageName) ?~ String (T.pack specifier)

    -- The build's package.json lives at `/app` in Docker and the generated
    -- app at `/app/.wasp/out`, like in the Wasp project.
    tarballSpecifierInDocker :: Path' Abs File' -> String
    tarballSpecifierInDocker tarball =
      tarballFileSpecifier $ fromRelFile $ generatedAppDirInWaspProjectDir </> devWaspNpmPackagesDirInGeneratedAppDir </> basename tarball

    -- The root dependency on @wasp/wasp and the `resolved` field of each
    -- package entry.
    pointNpmLockAtCopiedTarballs :: DevWaspNpmPackageTarballs -> Value -> Value
    pointNpmLockAtCopiedTarballs tarballs lock =
      lock
        & key "packages" . key "" . key "dependencies" . key (Key.fromString waspNpmPackageName) . _String %~ rewriteSpecifier
        & key "packages" . _Object %~ KM.map (key "resolved" . _String %~ rewriteSpecifier)
      where
        allTarballs = waspTarball tarballs : map snd (dependencyTarballs tarballs)

        rewriteSpecifier specifier =
          maybe specifier (T.pack . tarballSpecifierInDocker) $ find (pointsAt specifier) allTarballs

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
