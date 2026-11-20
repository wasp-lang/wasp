module Wasp.Project.WaspNpmPackage
  ( -- * Versions
    InstalledWaspNpmPackageError (..),
    tryGettingInstalledWaspNpmPackageVersion,
    doesVersionMatchDeclaredVersion,
    WaspNpmPackageDeclaration (..),
    getWaspNpmPackageDeclaration,
    getDeclaredVersion,
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
