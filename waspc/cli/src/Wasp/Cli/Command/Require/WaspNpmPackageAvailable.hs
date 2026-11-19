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
import Wasp.Cli.Command.Require.ValidNodeAndPackageManager (ValidNodeAndPackageManager (ValidNodeAndPackageManager))
import Wasp.Cli.Message (cliSendWarningToStderr)
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.PackageManager (PackageManager, detectPackageManagerOrDefault, makeInstallCommand, makeRunScriptCommand)
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
    ValidNodeAndPackageManager <- require
    cliVersion <- liftIO getWaspCliNpmPackageVersion
    packageManager <- liftIO $ detectPackageManagerOrDefault waspProjectDir
    declaredVersion <- getDeclaredWaspNpmPackageVersion waspProjectDir packageManager cliVersion
    installedVersion <- getInstalledWaspNpmPackageVersion waspProjectDir packageManager
    unless (doesVersionMatchDeclaredVersion declaredVersion installedVersion)
      $ throwError
      $ staleInstallError packageManager declaredVersion installedVersion
    when (cliVersion /= installedVersion) $ do
      isRunByNpmPackage <- liftIO isRunByWaspCliNpmPackage
      throwError $
        if isRunByNpmPackage
          then partialInstallError packageManager installedVersion cliVersion
          else directRunError packageManager cliVersion installedVersion
    return WaspNpmPackageAvailable

getDeclaredWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> PackageManager -> SV.Version -> Command String
getDeclaredWaspNpmPackageVersion waspProjectDir packageManager cliVersion =
  liftIO (getWaspNpmPackageDeclaration waspProjectDir) >>= \case
    Left parseError -> throwError $ CommandError "Couldn't read package.json" parseError
    Right (DeclaredInDependencies version) -> return version
    Right (DeclaredInDevDependencies version) -> do
      liftIO $ cliSendWarningToStderr inDevDependenciesWarningTitle inDevDependenciesWarning
      return version
    Right NotDeclared -> throwError $ notADependencyError packageManager cliVersion

getInstalledWaspNpmPackageVersion :: Path' Abs (Dir WaspProjectDir) -> PackageManager -> Command SV.Version
getInstalledWaspNpmPackageVersion waspProjectDir packageManager =
  liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
    Right version -> return version
    Left WaspNpmPackageNotInstalled -> throwError $ notInstalledError packageManager waspProjectDir
    Left (BrokenWaspNpmPackage reason) -> throwError $ brokenInstallError packageManager reason

notADependencyError :: PackageManager -> SV.Version -> CommandError
notADependencyError packageManager cliVersion =
  CommandError
    "Wasp is not a dependency of this project"
    $ "Add "
      ++ styleCode ("\"" ++ waspNpmPackageName ++ "\": \"" ++ show cliVersion ++ "\"")
      ++ " to the "
      ++ styleCode "dependencies"
      ++ " in your "
      ++ styleCode "package.json"
      ++ " and run "
      ++ styleCode (makeInstallCommand packageManager)
      ++ ". If you're upgrading from Wasp 0.26, follow the migration guide: "
      ++ "https://wasp.sh/docs/0.27/migration-guide#depend-on-wasp"

inDevDependenciesWarningTitle :: String
inDevDependenciesWarningTitle = "Wasp is a dev dependency of this project"

inDevDependenciesWarning :: String
inDevDependenciesWarning =
  "The "
    ++ styleCode waspNpmPackageName
    ++ " npm package brings your app's runtime dependencies, so installs that skip dev dependencies leave them out. Move it from "
    ++ styleCode "devDependencies"
    ++ " to "
    ++ styleCode "dependencies"
    ++ " in your "
    ++ styleCode "package.json"
    ++ "."

notInstalledError :: PackageManager -> Path' Abs (Dir WaspProjectDir) -> CommandError
notInstalledError packageManager waspProjectDir =
  CommandError "Wasp is not installed" $
    "Couldn't find "
      ++ styleCode waspNpmPackageName
      ++ " in the node_modules of "
      ++ fromAbsDir waspProjectDir
      ++ " or its parent dirs. Run "
      ++ styleCode (makeInstallCommand packageManager)
      ++ " to install it."

brokenInstallError :: PackageManager -> String -> CommandError
brokenInstallError packageManager reason =
  CommandError "The installed Wasp is broken" $
    "Run "
      ++ styleCode (makeInstallCommand packageManager)
      ++ " to reinstall it.\nDetails: "
      ++ reason

staleInstallError :: PackageManager -> String -> SV.Version -> CommandError
staleInstallError packageManager declaredVersion installedVersion =
  outOfDateError packageManager $
    "The package.json asks for "
      ++ styleCode (waspNpmPackageName ++ " " ++ declaredVersion)
      ++ ", but node_modules has "
      ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
      ++ "."

-- | E.g. after a partial install.
partialInstallError :: PackageManager -> SV.Version -> SV.Version -> CommandError
partialInstallError packageManager installedVersion cliVersion =
  outOfDateError packageManager $
    "node_modules has "
      ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
      ++ ", but "
      ++ styleCode (waspCliNpmPackageName ++ " " ++ show cliVersion)
      ++ "."

outOfDateError :: PackageManager -> String -> CommandError
outOfDateError packageManager reason =
  CommandError "The installed Wasp is out of date" $
    reason ++ " Run " ++ styleCode (makeInstallCommand packageManager) ++ " to update it."

-- | Only the \@wasp/cli npm package hands over to the project's Wasp.
directRunError :: PackageManager -> SV.Version -> SV.Version -> CommandError
directRunError packageManager cliVersion installedVersion =
  CommandError "Wasp version mismatch" $
    "You ran the Wasp binary "
      ++ show cliVersion
      ++ " directly, e.g. one built from source, so it can't switch to this project's Wasp "
      ++ show installedVersion
      ++ ". Run the project's Wasp with "
      ++ styleCode (makeRunScriptCommand packageManager "wasp" ["<command>"])
      ++ "."
