module Wasp.Project.WaspNpmPackage
  ( -- * Versions
    InstalledWaspNpmPackageError (..),
    tryGettingInstalledWaspNpmPackageVersion,
    doesVersionMatchDeclaredVersion,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,

    -- * Scripts
    WaspNpmPackageScript (..),
    waspNpmPackageScriptCommand,
    runWaspNpmPackageScript,
  )
where

import Data.Bifunctor (first)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (trim)
import qualified Wasp.Version as WV

data InstalledWaspNpmPackageError
  = WaspNpmPackageNotInstalled
  | -- | Its package.json is unreadable or has no valid version, with this reason.
    BrokenWaspNpmPackage String

-- | Asks Node, so the CLI finds the same \@wasp/wasp that Node and \@wasp/cli do.
tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either InstalledWaspNpmPackageError SV.Version)
tryGettingInstalledWaspNpmPackageVersion waspProjectDir = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode printWaspNpmPackageVersionProcess ""
  return $ case exitCode of
    ExitSuccess -> case trim stdout of
      "undefined" -> Left $ BrokenWaspNpmPackage "Its package.json has no `version` field."
      version -> first (BrokenWaspNpmPackage . show) $ SV.parseVersion version
    ExitFailure _
      | "MODULE_NOT_FOUND" `isInfixOf` stderr -> Left WaspNpmPackageNotInstalled
      | otherwise -> Left $ BrokenWaspNpmPackage $ fromMaybe (trim stderr) $ find ("Error" `isInfixOf`) $ lines stderr
  where
    printWaspNpmPackageVersionProcess =
      (P.proc "node" ["--print", "require(" ++ show (waspNpmPackageName ++ "/package.json") ++ ").version"])
        { P.cwd = Just $ fromAbsDir waspProjectDir
        }

-- | Matches like npm. A prerelease only matches a range that names a prerelease
-- of the same version: `^0.27.0-rc.1` matches 0.27.0-rc.2, `^0.27.0` doesn't.
-- A declared version that isn't a range, e.g. a `file:` tarball, matches anything.
doesVersionMatchDeclaredVersion :: String -> SV.Version -> Bool
doesVersionMatchDeclaredVersion declaredVersion version =
  either (const True) (SV.isVersionInRange version) (SV.parseRange declaredVersion)

-- \@wasp/cli tells this binary its version through an env var, because Cabal
-- versions can't express npm prereleases like `0.27.0-rc.1`.

-- | Falls back to the binary's own version when \@wasp/cli doesn't run it.
getWaspCliNpmPackageVersion :: IO SV.Version
getWaspCliNpmPackageVersion = fromMaybe WV.waspVersion <$> lookupWaspCliNpmPackageVersion

-- | A binary run directly, e.g. one built from source, can't hand over to the
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

-- | The scripts \@wasp/wasp exports for the CLI to run.
data WaspNpmPackageScript
  = WaspSpecAnalyzerScript
  | WaspDeployScript
  | WaspStudioScript

waspNpmPackageScriptExportName :: WaspNpmPackageScript -> String
waspNpmPackageScriptExportName = \case
  WaspSpecAnalyzerScript -> "run-spec"
  WaspDeployScript -> "run-deploy"
  WaspStudioScript -> "run-studio"

-- | Run from the project dir, Node resolves the script through \@wasp/wasp's
-- `exports`. Requires the scripts to handle `--eval` arguments, which have no
-- script path in `process.argv`.
waspNpmPackageScriptCommand :: WaspNpmPackageScript -> [String] -> NodeCommand WaspProjectDir
waspNpmPackageScriptCommand script args =
  NodeCommand "node" $ ["--input-type=module", "--eval", "import " ++ show scriptImport ++ ";", "--"] ++ args
  where
    scriptImport = waspNpmPackageName ++ "/" ++ waspNpmPackageScriptExportName script

runWaspNpmPackageScript :: Path' Abs (Dir WaspProjectDir) -> WaspNpmPackageScript -> [String] -> IO ExitCode
runWaspNpmPackageScript waspProjectDir script args =
  P.withCreateProcess scriptProcess $ \_ _ _ processHandle -> P.waitForProcess processHandle
  where
    NodeCommand executable executableArgs = waspNpmPackageScriptCommand script args

    -- Attached to this terminal, so Ctrl+C reaches the script and it shuts
    -- down properly (see `delegate_ctlc` in
    -- https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html#g:4).
    scriptProcess =
      (P.proc executable executableArgs)
        { P.cwd = Just $ fromAbsDir waspProjectDir,
          P.std_in = P.Inherit,
          P.std_out = P.Inherit,
          P.std_err = P.Inherit,
          P.delegate_ctlc = True
        }
