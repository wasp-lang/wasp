module Wasp.Cli.Command.Require.WaspNpmPackageAvailable
  ( WaspNpmPackageAvailable (WaspNpmPackageAvailable),
  )
where

import Control.Monad (unless, when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import Wasp.Cli.Command (Command, CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Message (cliSendWarningToStderr)
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.WaspNpmPackage
  ( InstalledWaspNpmPackageError (..),
    WaspNpmPackageDeclaration (..),
    doesVersionMatchDeclaredVersion,
    getWaspNpmPackageDeclaration,
    tryGettingInstalledWaspNpmPackageVersion,
  )
import qualified Wasp.SemanticVersion as SV
import Wasp.Util.Terminal (styleCode)
import Wasp.Version (getWaspCliNpmPackageVersion, isRunByWaspCliNpmPackage)

-- | Require that the project's \@wasp/wasp npm package is installed in
-- node_modules, that it satisfies the version the project asks for, and that
-- this CLI has the same version. Warns when it's a dev dependency.
data WaspNpmPackageAvailable = WaspNpmPackageAvailable deriving (Typeable)

instance Requirable WaspNpmPackageAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Commands that require this read the Wasp Spec, which runs Node.js, so
    -- they also need Node.js and npm.
    ValidNodeAndNpm <- require
    cliVersion <- liftIO getWaspCliNpmPackageVersion
    declaredVersion <- getDeclaredWaspNpmPackageVersion waspProjectDir cliVersion
    installedVersion <- getInstalledWaspNpmPackageVersion waspProjectDir
    unless (doesVersionMatchDeclaredVersion declaredVersion installedVersion)
      $ throwError
      $ staleInstallError declaredVersion installedVersion
    when (cliVersion /= installedVersion) $ do
      isRunByNpmPackage <- liftIO isRunByWaspCliNpmPackage
      throwError $
        if isRunByNpmPackage
          then partialInstallError installedVersion cliVersion
          else directRunError cliVersion installedVersion
    return WaspNpmPackageAvailable

getDeclaredWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> SV.Version -> Command String
getDeclaredWaspNpmPackageVersion waspProjectDir cliVersion =
  liftIO (getWaspNpmPackageDeclaration waspProjectDir) >>= \case
    Left parseError -> throwError $ CommandError "Couldn't read package.json" parseError
    Right (DeclaredInDependencies version) -> return version
    Right (DeclaredInDevDependencies version) -> do
      liftIO $ cliSendWarningToStderr inDevDependenciesWarningTitle inDevDependenciesWarning
      return version
    Right NotDeclared -> throwError $ notADependencyError cliVersion

getInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> Command SV.Version
getInstalledWaspNpmPackageVersion waspProjectDir =
  liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
    Right version -> return version
    Left WaspNpmPackageNotInstalled -> throwError $ notInstalledError waspProjectDir
    Left (BrokenWaspNpmPackage reason) -> throwError $ brokenInstallError reason

notADependencyError :: SV.Version -> CommandError
notADependencyError cliVersion =
  CommandError
    "Wasp is not a dependency of this project"
    $ "Add "
      ++ styleCode ("\"" ++ waspNpmPackageName ++ "\": \"" ++ show cliVersion ++ "\"")
      ++ " to the "
      ++ styleCode "dependencies"
      ++ " in your "
      ++ styleCode "package.json"
      ++ " and run "
      ++ styleCode "npm install"
      ++ ". If you're upgrading from Wasp 0.26, follow the migration guide: "
      ++ "https://wasp.sh/docs/0.27/migration-guide#depend-on-wasp"

inDevDependenciesWarningTitle :: String
inDevDependenciesWarningTitle = "Wasp is a dev dependency of this project"

inDevDependenciesWarning :: String
inDevDependenciesWarning =
  "The "
    ++ styleCode waspNpmPackageName
    ++ " npm package brings your app's runtime dependencies, so installs that skip dev dependencies, like "
    ++ styleCode "npm ci --omit=dev"
    ++ ", leave them out. Move it from "
    ++ styleCode "devDependencies"
    ++ " to "
    ++ styleCode "dependencies"
    ++ " in your "
    ++ styleCode "package.json"
    ++ "."

notInstalledError :: Path' Abs (Dir WaspProjectDir) -> CommandError
notInstalledError waspProjectDir =
  CommandError "Wasp is not installed" $
    "Couldn't find "
      ++ styleCode waspNpmPackageName
      ++ " in the node_modules of "
      ++ fromAbsDir waspProjectDir
      ++ " or its parent dirs. Run "
      ++ styleCode "npm install"
      ++ " to install it."

brokenInstallError :: String -> CommandError
brokenInstallError reason =
  CommandError "The installed Wasp is broken" $
    "Run "
      ++ styleCode "npm install"
      ++ " to reinstall it.\nDetails: "
      ++ reason

staleInstallError :: String -> SV.Version -> CommandError
staleInstallError declaredVersion installedVersion =
  outOfDateError $
    "The package.json asks for "
      ++ styleCode (waspNpmPackageName ++ " " ++ declaredVersion)
      ++ ", but node_modules has "
      ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
      ++ "."

-- | E.g. after a partial install.
partialInstallError :: SV.Version -> SV.Version -> CommandError
partialInstallError installedVersion cliVersion =
  outOfDateError $
    "node_modules has "
      ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
      ++ ", but "
      ++ styleCode (waspCliNpmPackageName ++ " " ++ show cliVersion)
      ++ "."

outOfDateError :: String -> CommandError
outOfDateError reason =
  CommandError "The installed Wasp is out of date" $
    reason ++ " Run " ++ styleCode "npm install" ++ " to update it."

-- | Only the \@wasp/cli npm package hands over to the project's Wasp.
directRunError :: SV.Version -> SV.Version -> CommandError
directRunError cliVersion installedVersion =
  CommandError "Wasp version mismatch" $
    "You ran the Wasp binary "
      ++ show cliVersion
      ++ " directly, e.g. one built from source, so it can't switch to this project's Wasp "
      ++ show installedVersion
      ++ ". Run the project's Wasp with "
      ++ styleCode "npm run wasp -- <command>"
      ++ "."
