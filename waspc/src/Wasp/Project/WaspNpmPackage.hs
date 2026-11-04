module Wasp.Project.WaspNpmPackage
  ( -- * The Wasp npm package

    -- Wasp projects depend on Wasp through the @wasp/wasp npm package. It brings
    -- all the other core Wasp packages at its own version, and ships the
    -- scripts the CLI runs them with.
    waspNpmPackageName,
    waspCliNpmPackageName,
    waspSpecNpmPackageName,
    waspDeployNpmPackageName,
    waspStudioNpmPackageName,
    WaspNpmPackageDir,
    waspSpecAnalyzerScriptInWaspNpmPackageDir,
    waspDeployScriptInWaspNpmPackageDir,
    waspStudioScriptInWaspNpmPackageDir,
    findWaspNpmPackageScript,
    runWaspNpmPackageScript,
    tryGettingInstalledWaspNpmPackageVersion,
    NpmPackageVersion (..),
    parseNpmPackageVersion,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,
    doesVersionMatchDeclaredSpecifier,
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
import qualified Text.Parsec as Parsec
import qualified Wasp.ExternalConfig.Npm.PackageJson as PJ
import Wasp.Project.Common (WaspProjectDir)
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

-- | Finds the project's installed \@wasp/wasp the way Node.js resolves a
-- package: in the node_modules dir of the project, or of the closest parent
-- dir that has it. That covers every package manager's node_modules layout,
-- including monorepos where the package manager hoists dependencies to a
-- parent dir. The npm \@wasp/cli finds \@wasp/wasp with the same rule (see
-- `packages/cli/projectWaspCli.js`), so the two never disagree.
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

-- | The script that runs the Wasp Spec analyzer of the project's \@wasp/spec.
-- We run it with `node` by path. From inside \@wasp/wasp, Node resolves
-- \@wasp/spec next to it under any package manager.
waspSpecAnalyzerScriptInWaspNpmPackageDir :: Path' (Rel WaspNpmPackageDir) File'
waspSpecAnalyzerScriptInWaspNpmPackageDir = [relfile|run-spec.js|]

-- | The script that runs the project's \@wasp/deploy, by path like
-- 'waspSpecAnalyzerScriptInWaspNpmPackageDir'.
waspDeployScriptInWaspNpmPackageDir :: Path' (Rel WaspNpmPackageDir) File'
waspDeployScriptInWaspNpmPackageDir = [relfile|run-deploy.js|]

-- | The script that runs the project's \@wasp/studio, by path like
-- 'waspSpecAnalyzerScriptInWaspNpmPackageDir'.
waspStudioScriptInWaspNpmPackageDir :: Path' (Rel WaspNpmPackageDir) File'
waspStudioScriptInWaspNpmPackageDir = [relfile|run-studio.js|]

-- | The absolute path of one of the scripts the project's installed
-- \@wasp/wasp ships, like 'waspDeployScriptInWaspNpmPackageDir'. Fails when
-- \@wasp/wasp isn't installed, which commands that require
-- @WaspNpmPackageAvailable@ already checked.
findWaspNpmPackageScript :: Path' Abs (Dir WaspProjectDir) -> Path' (Rel WaspNpmPackageDir) File' -> IO (Either String (Path' Abs File'))
findWaspNpmPackageScript waspProjectDir script =
  maybe (Left $ waspNpmPackageNotInstalledMessage waspProjectDir) (Right . (</> script))
    <$> findWaspNpmPackageDir waspProjectDir

-- | Runs one of the scripts the project's \@wasp/wasp ships with `node` in the
-- project dir, attached to this terminal. Ctrl+C reaches the script, which
-- shuts it down properly. The calling command must require Node.js and the
-- project's installed dependencies (@ValidNodeAndNpm@ and
-- @WaspNpmPackageAvailable@).
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

tryGettingInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Either String NpmPackageVersion)
tryGettingInstalledWaspNpmPackageVersion waspProjectDir = runExceptT $ do
  waspNpmPackageDir <-
    ExceptT $ maybe (Left $ waspNpmPackageNotInstalledMessage waspProjectDir) Right <$> findWaspNpmPackageDir waspProjectDir
  let packageJsonPath :: Path' Abs (File InstalledPackageJsonFile)
      packageJsonPath = castFile $ waspNpmPackageDir </> [relfile|package.json|]
  packageJson <- ExceptT $ liftIO $ PJ.parsePackageJsonFile packageJsonPath
  ExceptT $ return $ case PJ.version packageJson of
    Just versionString -> parseNpmPackageVersion versionString
    Nothing -> Left $ fromAbsFile packageJsonPath ++ " has no `version` field"

waspNpmPackageNotInstalledMessage :: Path' Abs (Dir WaspProjectDir) -> String
waspNpmPackageNotInstalledMessage waspProjectDir =
  "Couldn't find "
    ++ waspNpmPackageName
    ++ " in the node_modules of "
    ++ fromAbsDir waspProjectDir
    ++ " or its parent dirs."

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
getWaspCliNpmPackageVersion = fromMaybe binaryVersion <$> lookupWaspCliNpmPackageVersion
  where
    binaryVersion = NpmPackageVersion WV.waspVersion False (show WV.waspVersion)

-- | Whether the \@wasp/cli npm package runs this binary. It doesn't when
-- someone runs the binary directly, e.g. one installed with the old installer
-- script or built from source. Then nothing hands over to the project's
-- Wasp CLI.
isRunByWaspCliNpmPackage :: IO Bool
isRunByWaspCliNpmPackage = isJust <$> lookupWaspCliNpmPackageVersion

-- | Fails on a version that doesn't parse, because then \@wasp/cli is broken.
lookupWaspCliNpmPackageVersion :: IO (Maybe NpmPackageVersion)
lookupWaspCliNpmPackageVersion =
  lookupEnv waspCliNpmPackageVersionEnvVarName
    >>= traverse (either (fail . invalidVersionMessage) return . parseNpmPackageVersion)
  where
    invalidVersionMessage parseError =
      waspCliNpmPackageVersionEnvVarName ++ " is not a valid version: " ++ parseError
