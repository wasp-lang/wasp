module Wasp.Project.WaspNpmPackage
  ( -- * The Wasp npm package
    WaspNpmPackageDir,

    -- * Versions
    InstalledWaspNpmPackageError (..),
    tryGettingInstalledWaspNpmPackageVersion,
    doesVersionMatchDeclaredSpecifier,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,

    -- * Scripts
    waspSpecAnalyzerScriptInWaspNpmPackageDir,
    waspDeployScriptInWaspNpmPackageDir,
    waspStudioScriptInWaspNpmPackageDir,
    findWaspNpmPackageScript,
    runWaspNpmPackageScript,
  )
where

import Control.Monad.Except (ExceptT (ExceptT), runExceptT)
import Control.Monad.Extra (findM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe, isJust)
import StrongPath (Abs, Dir, Dir', File, File', Path', Rel, castDir, castFile, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Environment (lookupEnv)
import System.Exit (ExitCode)
import qualified System.Process as P
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Version as WV

data WaspNpmPackageDir

data InstalledPackageJsonFile

instance PJ.PackageJsonFile InstalledPackageJsonFile

data InstalledWaspNpmPackageError
  = WaspNpmPackageNotInstalled
  | -- | Its package.json is unreadable or has no valid version, with this reason.
    BrokenWaspNpmPackage String

tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either InstalledWaspNpmPackageError SV.Version)
tryGettingInstalledWaspNpmPackageVersion waspProjectDir = runExceptT $ do
  waspNpmPackageDir <-
    ExceptT $ maybe (Left WaspNpmPackageNotInstalled) Right <$> findWaspNpmPackageDir waspProjectDir
  let packageJsonPath :: Path' Abs (File InstalledPackageJsonFile)
      packageJsonPath = castFile $ waspNpmPackageDir </> [relfile|package.json|]
  packageJson <- ExceptT $ liftIO $ first BrokenWaspNpmPackage <$> PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ first BrokenWaspNpmPackage $ case PJ.version packageJson of
    Just versionString -> first show $ SV.parseVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"

-- | Matches like npm: a range never matches a prerelease, and a specifier that
-- isn't a version, e.g. a `file:` tarball, matches anything.
doesVersionMatchDeclaredSpecifier :: String -> SV.Version -> Bool
doesVersionMatchDeclaredSpecifier declaredSpecifier version =
  either (const True) (SV.isVersionInRange version) (SV.parseRange declaredSpecifier)

-- \@wasp/cli tells this binary its version through an env var, because Cabal
-- versions can't express npm prereleases like `0.27.0-rc.1`.

-- | Falls back to the binary's own version when \@wasp/cli doesn't run it.
getWaspCliNpmPackageVersion :: IO SV.Version
getWaspCliNpmPackageVersion = fromMaybe WV.waspVersion <$> lookupWaspCliNpmPackageVersion

-- | A binary run directly, e.g. by the legacy installer, can't hand over to the
-- project's Wasp, so errors advise it differently.
isRunByWaspCliNpmPackage :: IO Bool
isRunByWaspCliNpmPackage = isJust <$> lookupWaspCliNpmPackageVersion

-- | The env var that \@wasp/cli sets its version to.
waspCliNpmPackageVersionEnvVarName :: String
waspCliNpmPackageVersionEnvVarName = "WASP_CLI_NPM_PACKAGE_VERSION"

-- | Fails on a version that doesn't parse, which should never happen.
lookupWaspCliNpmPackageVersion :: IO (Maybe SV.Version)
lookupWaspCliNpmPackageVersion =
  lookupEnv waspCliNpmPackageVersionEnvVarName
    >>= traverse (either (fail . invalidVersionMessage . show) return . SV.parseVersion)
  where
    invalidVersionMessage parseError =
      waspCliNpmPackageVersionEnvVarName ++ " is not a valid version: " ++ parseError

-- | We run the scripts with `node` by path, so Node resolves the packages they
-- import from inside \@wasp/wasp, under any package manager.
waspSpecAnalyzerScriptInWaspNpmPackageDir :: Path' (Rel WaspNpmPackageDir) File'
waspSpecAnalyzerScriptInWaspNpmPackageDir = [relfile|scripts/run-spec.js|]

waspDeployScriptInWaspNpmPackageDir :: Path' (Rel WaspNpmPackageDir) File'
waspDeployScriptInWaspNpmPackageDir = [relfile|scripts/run-deploy.js|]

waspStudioScriptInWaspNpmPackageDir :: Path' (Rel WaspNpmPackageDir) File'
waspStudioScriptInWaspNpmPackageDir = [relfile|scripts/run-studio.js|]

-- | Fails when \@wasp/wasp isn't installed, which @WaspNpmPackageAvailable@
-- already checks.
findWaspNpmPackageScript :: Path' Abs (Dir WaspProjectDir) -> Path' (Rel WaspNpmPackageDir) File' -> IO (Either String (Path' Abs File'))
findWaspNpmPackageScript waspProjectDir script =
  maybe (Left $ waspNpmPackageNotInstalledMessage waspProjectDir) (Right . (</> script))
    <$> findWaspNpmPackageDir waspProjectDir

-- | Runs the script attached to this terminal, so Ctrl+C reaches it and it
-- shuts down properly. Needs @ValidNodeAndNpm@ and @WaspNpmPackageAvailable@.
runWaspNpmPackageScript :: Path' Abs (Dir WaspProjectDir) -> Path' (Rel WaspNpmPackageDir) File' -> [String] -> IO (Either String ExitCode)
runWaspNpmPackageScript waspProjectDir script args =
  findWaspNpmPackageScript waspProjectDir script >>= traverse runScript
  where
    runScript scriptPath =
      P.withCreateProcess (scriptProcess scriptPath) $ \_ _ _ processHandle -> P.waitForProcess processHandle

    -- See https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html#g:4.
    scriptProcess scriptPath =
      (P.proc "node" (fromAbsFile scriptPath : args))
        { P.cwd = Just $ fromAbsDir waspProjectDir,
          P.std_in = P.Inherit,
          P.std_out = P.Inherit,
          P.std_err = P.Inherit,
          P.delegate_ctlc = True
        }

-- | Finds the project's \@wasp/wasp with Node's resolution algorithm.
findWaspNpmPackageDir :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' Abs (Dir WaspNpmPackageDir)))
findWaspNpmPackageDir waspProjectDir =
  findM (IOUtil.doesFileExist . (</> [relfile|package.json|])) $
    waspNpmPackageDirCandidates (castDir waspProjectDir)
  where
    waspNpmPackageDirCandidates :: Path' Abs Dir' -> [Path' Abs (Dir WaspNpmPackageDir)]
    waspNpmPackageDirCandidates dir =
      (dir </> [reldir|node_modules/@wasp/wasp|])
        : (if parentDir == dir then [] else waspNpmPackageDirCandidates parentDir)
      where
        parentDir = SP.parent dir

waspNpmPackageNotInstalledMessage :: Path' Abs (Dir WaspProjectDir) -> String
waspNpmPackageNotInstalledMessage waspProjectDir =
  "Couldn't find "
    ++ waspNpmPackageName
    ++ " in the node_modules of "
    ++ fromAbsDir waspProjectDir
    ++ " or its parent dirs."
