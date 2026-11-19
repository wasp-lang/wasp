module Wasp.Project.WaspNpmPackage
  ( -- * Versions
    InstalledWaspNpmPackageError (..),
    tryGettingInstalledWaspNpmPackageVersion,
    tryGettingInstalledWaspNpmPackagePeerDeps,
    doesVersionMatchDeclaredVersion,
    WaspNpmPackageDeclaration (..),
    getWaspNpmPackageDeclaration,
    getDeclaredVersion,
    dependsOnDevWaspNpmPackages,

    -- * Scripts
    WaspNpmPackageScript (..),
    waspNpmPackageScriptCommand,
    runWaspNpmPackageScript,
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (find, isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Generator.NodeCommand (NodeCommand (..))
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util (trim)

data InstalledWaspNpmPackageError
  = WaspNpmPackageNotInstalled
  | -- | Its package.json is unreadable or has an invalid field, for this reason.
    BrokenWaspNpmPackage String

tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either InstalledWaspNpmPackageError SV.Version)
tryGettingInstalledWaspNpmPackageVersion waspProjectDir =
  (>>= parseVersion) <$> readInstalledWaspNpmPackageJson waspProjectDir (++ ".version")
  where
    parseVersion = \case
      "undefined" -> Left $ BrokenWaspNpmPackage "Its package.json has no `version` field."
      version -> first (BrokenWaspNpmPackage . show) $ SV.parseVersion version

-- | The packages Wasp needs in every app, e.g. React or Express. Only the
-- package.json of \@wasp/wasp lists them.
tryGettingInstalledWaspNpmPackagePeerDeps :: Path' Abs (Dir WaspProjectDir) -> IO (Either InstalledWaspNpmPackageError [Npm.Dependency.Dependency])
tryGettingInstalledWaspNpmPackagePeerDeps waspProjectDir =
  (>>= parsePeerDeps) <$> readInstalledWaspNpmPackageJson waspProjectDir peerDepsAsJson
  where
    peerDepsAsJson packageJson = "JSON.stringify(" ++ packageJson ++ ".peerDependencies ?? {})"
    parsePeerDeps =
      first BrokenWaspNpmPackage
        . fmap (Npm.Dependency.fromList . M.toList)
        . Aeson.eitherDecode
        . BLU.fromString

-- | Reads the project's installed \@wasp/wasp's package.json through Node, so
-- the CLI sees the same \@wasp/wasp as the project's code. Keep in sync with
-- `findProjectWaspCli` in @packages/cli/src/find-project-wasp-cli.ts@.
readInstalledWaspNpmPackageJson :: Path' Abs (Dir WaspProjectDir) -> (String -> String) -> IO (Either InstalledWaspNpmPackageError String)
readInstalledWaspNpmPackageJson waspProjectDir makeJsExpression = do
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

-- | A declared version that isn't a range, e.g. a `file:` tarball, matches anything.
doesVersionMatchDeclaredVersion :: String -> SV.Version -> Bool
doesVersionMatchDeclaredVersion declaredVersion version =
  either (const True) (SV.isVersionInRange version) (SV.parseRange declaredVersion)

-- | Where the user's package.json lists \@wasp/wasp, if anywhere.
data WaspNpmPackageDeclaration
  = DeclaredInDependencies String
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

-- | TODO(#2262): Once Wasp works with pnpm, the repo's Wasp apps stop
-- depending on dev tarballs, and only projects that the dev CLI creates
-- outside the repo do.
--
-- Whether the project depends on \@wasp/wasp through a `file:` tarball. Only
-- projects that develop Wasp itself do (see @Wasp.Cli.DevWaspNpmPackages@).
dependsOnDevWaspNpmPackages :: PJ.PackageJson -> Bool
dependsOnDevWaspNpmPackages packageJson =
  maybe False ("file:" `isPrefixOf`) $
    M.lookup waspNpmPackageName (PJ.dependencies packageJson)

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

-- | Runs the project's \@wasp/wasp script. The scripts must handle `--eval`
-- arguments, which have no script path in `process.argv`.
waspNpmPackageScriptCommand :: WaspNpmPackageScript -> [String] -> NodeCommand WaspProjectDir
waspNpmPackageScriptCommand script args =
  NodeCommand "node" $ ["--input-type=module", "--eval", "import " ++ show scriptImport ++ ";", "--"] ++ args
  where
    scriptImport = waspNpmPackageName ++ "/" ++ scriptExportName

    scriptExportName :: String
    scriptExportName = case script of
      WaspSpecAnalyzerScript -> "internal/run-spec"
      WaspDeployScript -> "internal/run-deploy"
      WaspStudioScript -> "internal/run-studio"

runWaspNpmPackageScript :: Path' Abs (Dir WaspProjectDir) -> WaspNpmPackageScript -> [String] -> IO ExitCode
runWaspNpmPackageScript waspProjectDir script args =
  P.withCreateProcess scriptProcess $ \_ _ _ processHandle -> P.waitForProcess processHandle
  where
    NodeCommand executable executableArgs = waspNpmPackageScriptCommand script args

    -- Attached to this terminal, so Ctrl+C reaches the script, and Wasp waits
    -- for it to exit (see `delegate_ctlc` in
    -- https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html#g:4).
    scriptProcess =
      (P.proc executable executableArgs)
        { P.cwd = Just $ fromAbsDir waspProjectDir,
          P.std_in = P.Inherit,
          P.std_out = P.Inherit,
          P.std_err = P.Inherit,
          P.delegate_ctlc = True
        }
