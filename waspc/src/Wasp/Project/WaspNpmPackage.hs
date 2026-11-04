module Wasp.Project.WaspNpmPackage
  ( -- * The Wasp npm package

    -- Wasp projects depend on Wasp through the @wasp/wasp npm package. It pins
    -- the Wasp CLI (@wasp/cli), the Wasp Spec (@wasp/spec), @wasp/deploy and
    -- @wasp/studio to one version, and ships the scripts the CLI runs them with.
    waspNpmPackageName,
    waspCliNpmPackageName,
    waspSpecNpmPackageName,
    waspDeployNpmPackageName,
    waspStudioNpmPackageName,
    waspSpecAnalyzerScriptInWaspProjectDir,
    waspDeployScriptInWaspProjectDir,
    waspStudioScriptInWaspProjectDir,
    waspNpmPackageScriptProcess,
    tryGettingInstalledWaspNpmPackageVersion,
    NpmPackageVersion (..),
    parseNpmPackageVersion,
    waspCliNpmPackageVersionEnvVarName,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,
    doesVersionMatchDeclaredSpecifier,
    isCliCompatibleWithInstalledVersion,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Extra (unlessM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (fromMaybe, isJust)
import StrongPath (Abs, Dir, File, File', Path', Rel, castFile, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import System.Environment (lookupEnv)
import qualified System.Process as P
import qualified Text.Parsec as Parsec
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Project.Common (WaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Version as WV

waspNpmPackageName :: PJ.PackageName
waspNpmPackageName = "@wasp/wasp"

waspCliNpmPackageName :: PJ.PackageName
waspCliNpmPackageName = "@wasp/cli"

waspSpecNpmPackageName :: PJ.PackageName
waspSpecNpmPackageName = "@wasp/spec"

waspDeployNpmPackageName :: PJ.PackageName
waspDeployNpmPackageName = "@wasp/deploy"

waspStudioNpmPackageName :: PJ.PackageName
waspStudioNpmPackageName = "@wasp/studio"

data WaspNpmPackageDir

-- | \@wasp/wasp is a direct dependency of the project, so it is always at the
-- top level of node_modules, no matter which package manager laid it out.
waspNpmPackageDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir WaspNpmPackageDir)
waspNpmPackageDirInWaspProjectDir = nodeModulesDirInWaspProjectDir </> [reldir|@wasp/wasp|]

-- | The script that runs the Wasp Spec analyzer of the project's \@wasp/spec.
-- We run it with `node` by path. From inside \@wasp/wasp, Node resolves
-- \@wasp/spec next to it under any package manager.
waspSpecAnalyzerScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspSpecAnalyzerScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-spec.js|]

-- | The script that runs the project's \@wasp/deploy, by path like
-- 'waspSpecAnalyzerScriptInWaspProjectDir'.
waspDeployScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspDeployScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-deploy.js|]

-- | The script that runs the project's \@wasp/studio, by path like
-- 'waspSpecAnalyzerScriptInWaspProjectDir'.
waspStudioScriptInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
waspStudioScriptInWaspProjectDir = waspNpmPackageDirInWaspProjectDir </> [relfile|run-studio.js|]

-- | A process that runs one of the scripts \@wasp/wasp ships, like
-- 'waspDeployScriptInWaspProjectDir', with `node` in the project dir. The
-- calling command must require Node.js and the project's installed
-- dependencies (@ValidNodeAndNpm@ and @WaspNpmPackageAvailable@).
waspNpmPackageScriptProcess :: Path' Abs (Dir WaspProjectDir) -> Path' (Rel WaspProjectDir) File' -> [String] -> P.CreateProcess
waspNpmPackageScriptProcess waspProjectDir script args =
  (P.proc "node" (fromAbsFile (waspProjectDir </> script) : args))
    { P.cwd = Just $ fromAbsDir waspProjectDir
    }

tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either String NpmPackageVersion)
tryGettingInstalledWaspNpmPackageVersion projectDir = runExceptT $ do
  unlessM (liftIO $ IOUtil.doesFileExist packageJsonPath)
    $ throwError
    $ "Couldn't find " ++ fromAbsFile packageJsonPath
  packageJson <- ExceptT $ liftIO $ PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ case PJ.version packageJson of
    Just versionString -> parseNpmPackageVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"
  where
    packageJsonPath :: Path' Abs (File InstalledPackageJsonFile)
    packageJsonPath =
      castFile $ projectDir </> waspNpmPackageDirInWaspProjectDir </> [relfile|package.json|]

data InstalledPackageJsonFile

instance PJ.PackageJsonFile InstalledPackageJsonFile

-- | The version of an npm package, e.g. `0.27.0`, or `0.27.0-rc.1` for a
-- release candidate. 'SV.Version' has no prerelease part, so we keep its core
-- next to the full version.
data NpmPackageVersion = NpmPackageVersion
  { versionCore :: SV.Version,
    -- | Whether the version has a prerelease part, like `-rc.1`.
    isPrerelease :: Bool,
    fullVersion :: String
  }
  deriving (Eq)

instance Show NpmPackageVersion where
  show = fullVersion

-- | Parses a SemVer version with an optional prerelease and build part.
-- See: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
parseNpmPackageVersion :: String -> Either String NpmPackageVersion
parseNpmPackageVersion versionString =
  first show $ Parsec.parse (npmPackageVersionParser <* Parsec.eof) "" versionString
  where
    npmPackageVersionParser = do
      core <- SV.versionParser
      prerelease <- Parsec.optionMaybe (Parsec.char '-' *> dotSeparatedIdentifiersParser)
      _build <- Parsec.optionMaybe (Parsec.char '+' *> dotSeparatedIdentifiersParser)
      return $ NpmPackageVersion core (isJust prerelease) versionString

    dotSeparatedIdentifiersParser =
      Parsec.many1 (Parsec.alphaNum Parsec.<|> Parsec.char '-') `Parsec.sepBy1` Parsec.char '.'

-- | Whether the version satisfies what the project's package.json declares for
-- \@wasp/wasp, the way npm decides it:
--
-- - A version range (e.g. `^0.27.0`) never matches a prerelease, because Wasp's
--   ranges have no prerelease part.
-- - An exact prerelease version (e.g. `0.27.0-rc.1`) matches only itself.
-- - Other specifiers, e.g. `file:` tarballs, don't constrain the version.
doesVersionMatchDeclaredSpecifier :: String -> NpmPackageVersion -> Bool
doesVersionMatchDeclaredSpecifier declaredSpecifier version =
  case SV.parseRange declaredSpecifier of
    Right range -> not (isPrerelease version) && SV.isVersionInRange (versionCore version) range
    Left _ -> case parseNpmPackageVersion declaredSpecifier of
      Right declaredVersion -> fullVersion declaredVersion == fullVersion version
      Left _ -> True

-- | The env var that \@wasp/cli sets to its package version when it runs this
-- binary.
waspCliNpmPackageVersionEnvVarName :: String
waspCliNpmPackageVersionEnvVarName = "WASP_CLI_NPM_PACKAGE_VERSION"

-- | The version of the \@wasp/cli npm package that runs this binary, e.g.
-- `0.27.0-rc.1`. It's the source of truth for the Wasp versions we write into
-- package.json files, because the binary's own version ('WV.waspVersion') comes
-- from Cabal, whose version format is incompatible with npm's SemVer. Falls back
-- to the binary's version when the binary runs without \@wasp/cli, e.g. when
-- built from source.
getWaspCliNpmPackageVersion :: IO NpmPackageVersion
getWaspCliNpmPackageVersion =
  fromMaybe binaryVersion . (>>= eitherToMaybe . parseNpmPackageVersion)
    <$> lookupEnv waspCliNpmPackageVersionEnvVarName
  where
    binaryVersion = NpmPackageVersion WV.waspVersion False (show WV.waspVersion)

-- | Whether the \@wasp/cli npm package runs this binary. It doesn't when
-- someone runs the binary directly, e.g. one installed with the old installer
-- script or built from source. Then nothing hands over to the project's
-- Wasp CLI.
isRunByWaspCliNpmPackage :: IO Bool
isRunByWaspCliNpmPackage = isJust <$> lookupEnv waspCliNpmPackageVersionEnvVarName

-- | Whether the CLI with the given version works with the project's installed
-- \@wasp/wasp. When the project declares a version or a range, the CLI's
-- version must match it like the installed version must. Otherwise, e.g. for a
-- `file:` tarball, the CLI's version must equal the installed version.
isCliCompatibleWithInstalledVersion :: NpmPackageVersion -> String -> NpmPackageVersion -> Bool
isCliCompatibleWithInstalledVersion cliVersion declaredSpecifier installedVersion
  | isVersionSpecifier = doesVersionMatchDeclaredSpecifier declaredSpecifier cliVersion
  | otherwise = fullVersion cliVersion == fullVersion installedVersion
  where
    isVersionSpecifier =
      isRight (SV.parseRange declaredSpecifier) || isRight (parseNpmPackageVersion declaredSpecifier)
