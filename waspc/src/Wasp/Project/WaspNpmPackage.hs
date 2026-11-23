module Wasp.Project.WaspNpmPackage
  ( -- * Versions
    InstalledWaspNpmPackageError (..),
    tryGettingInstalledWaspNpmPackageVersion,
    doesVersionMatchDeclaredVersion,
    WaspNpmPackageDeclaration (..),
    getWaspNpmPackageDeclaration,
    getDeclaredVersion,

    -- * Scripts
    WaspNpmPackageScript (..),
    waspNpmPackageScriptCommand,
    runWaspNpmPackageScript,
  )
where

import Data.Bifunctor (first)
import Data.List (find, isInfixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (trim)

data InstalledWaspNpmPackageError
  = WaspNpmPackageNotInstalled
  | -- | Its package.json is unreadable or has an invalid field, with this reason.
    BrokenWaspNpmPackage String

tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either InstalledWaspNpmPackageError SV.Version)
tryGettingInstalledWaspNpmPackageVersion waspProjectDir =
  (>>= parseVersion) <$> evaluateOnInstalledWaspNpmPackageJson waspProjectDir (++ ".version")
  where
    parseVersion = \case
      "undefined" -> Left $ BrokenWaspNpmPackage "Its package.json has no `version` field."
      version -> first (BrokenWaspNpmPackage . show) $ SV.parseVersion version

-- | Evaluates, in Node, the JS expression that the function builds from the JS
-- expression of the installed \@wasp/wasp's package.json, and returns its
-- printed value. Asks Node, so the CLI finds the same \@wasp/wasp that Node and
-- \@wasp/cli do.
evaluateOnInstalledWaspNpmPackageJson :: Path' Abs (Dir WaspProjectDir) -> (String -> String) -> IO (Either InstalledWaspNpmPackageError String)
evaluateOnInstalledWaspNpmPackageJson waspProjectDir makeJsExpression = do
  (exitCode, stdout, stderr) <- P.readCreateProcessWithExitCode evaluateProcess ""
  return $ case exitCode of
    ExitSuccess -> Right $ trim stdout
    ExitFailure _
      | "MODULE_NOT_FOUND" `isInfixOf` stderr -> Left WaspNpmPackageNotInstalled
      | otherwise -> Left $ BrokenWaspNpmPackage $ fromMaybe (trim stderr) $ find ("Error" `isInfixOf`) $ lines stderr
  where
    evaluateProcess =
      (P.proc "node" ["--print", makeJsExpression packageJsonJsExpression])
        { P.cwd = Just $ fromAbsDir waspProjectDir
        }
    packageJsonJsExpression = "require(" ++ show (waspNpmPackageName ++ "/package.json") ++ ")"

-- | Matches like npm. A prerelease only matches a range that names a prerelease
-- of the same version: `^0.27.0-rc.1` matches 0.27.0-rc.2, `^0.27.0` doesn't.
-- A declared version that isn't a range, e.g. a `file:` tarball, matches anything.
doesVersionMatchDeclaredVersion :: String -> SV.Version -> Bool
doesVersionMatchDeclaredVersion declaredVersion version =
  either (const True) (SV.isVersionInRange version) (SV.parseRange declaredVersion)

-- | Where the user's package.json lists \@wasp/wasp, if anywhere.
data WaspNpmPackageDeclaration
  = -- | Listed in `dependencies`, with this version.
    DeclaredInDependencies String
  | DeclaredInDevDependencies String
  | NotDeclared

getWaspNpmPackageDeclaration :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspNpmPackageDeclaration)
getWaspNpmPackageDeclaration waspProjectDir =
  findUserPackageJsonFile waspProjectDir >>= \case
    Nothing -> return $ Right NotDeclared
    Just packageJsonFile ->
      fmap getDeclaration <$> PJ.parsePackageJsonFile packageJsonFile
  where
    getDeclaration packageJson =
      case ( M.lookup waspNpmPackageName (PJ.dependencies packageJson),
             M.lookup waspNpmPackageName (PJ.devDependencies packageJson)
           ) of
        (Just version, _) -> DeclaredInDependencies version
        (Nothing, Just version) -> DeclaredInDevDependencies version
        (Nothing, Nothing) -> NotDeclared

getDeclaredVersion :: WaspNpmPackageDeclaration -> Maybe String
getDeclaredVersion = \case
  DeclaredInDependencies version -> Just version
  DeclaredInDevDependencies version -> Just version
  NotDeclared -> Nothing

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
