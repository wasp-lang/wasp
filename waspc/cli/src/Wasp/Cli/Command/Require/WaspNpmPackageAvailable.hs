module Wasp.Cli.Command.Require.WaspNpmPackageAvailable
  ( WaspNpmPackageAvailable (WaspNpmPackageAvailable),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import qualified Data.Map as M
import StrongPath (Abs, Dir, Path', fromAbsDir)
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.NpmPackageNames (waspNpmPackageName)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Project.WaspNpmPackage
  ( InstalledWaspNpmPackageError (..),
    doesVersionMatchDeclaredVersion,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,
    tryGettingInstalledWaspNpmPackageVersion,
  )
import qualified Wasp.SemanticVersion as SV
import Wasp.Util.Terminal (styleCode)

-- | Require that the project's \@wasp/wasp npm package is installed in
-- node_modules, that it satisfies the version the project asks for, and that
-- this CLI has the same version.
data WaspNpmPackageAvailable = WaspNpmPackageAvailable deriving (Typeable)

instance Requirable WaspNpmPackageAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Commands that require this read the Wasp Spec, which runs Node.js, so
    -- they also need Node.js and npm.
    ValidNodeAndNpm <- require
    cliVersion <- liftIO getWaspCliNpmPackageVersion
    declaredVersion <-
      liftIO (getWaspNpmPackageDeclaration waspProjectDir)
        >>= either (throwError . CommandError "Couldn't read package.json") return
        >>= \case
          DeclaredInDependencies version -> return version
          DeclaredInDevDependencies -> throwError inDevDependenciesError
          NotDeclared -> throwError $ notADependencyError cliVersion
    liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
      Right installedVersion
        | not (doesVersionMatchDeclaredVersion declaredVersion installedVersion) ->
            throwError $ staleInstallError declaredVersion installedVersion
        | cliVersion /= installedVersion -> do
            isRunByNpmPackage <- liftIO isRunByWaspCliNpmPackage
            throwError $ versionMismatchError isRunByNpmPackage cliVersion installedVersion
        | otherwise -> return WaspNpmPackageAvailable
      Left WaspNpmPackageNotInstalled -> throwError $ notInstalledError waspProjectDir
      Left (BrokenWaspNpmPackage reason) -> throwError $ brokenInstallError reason
    where
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

      staleInstallError :: String -> SV.Version -> CommandError
      staleInstallError declaredVersion installedVersion =
        CommandError "The installed Wasp is out of date" $
          "The package.json asks for "
            ++ styleCode (waspNpmPackageName ++ " " ++ declaredVersion)
            ++ ", but node_modules has "
            ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
            ++ ". Run "
            ++ styleCode "npm install"
            ++ " to update it."

      brokenInstallError :: String -> CommandError
      brokenInstallError reason =
        CommandError "The installed Wasp is broken" $
          "Run "
            ++ styleCode "npm install"
            ++ " to reinstall it.\nDetails: "
            ++ reason

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
            ++ "https://wasp.sh/docs/0.27/migration-guide#2-depend-on-waspwasp-instead-of-waspshspec"

      inDevDependenciesError =
        CommandError
          "Wasp is a dev dependency of this project"
          $ "The "
            ++ styleCode waspNpmPackageName
            ++ " npm package brings your app's runtime dependencies, so it must be a regular dependency. Move it from "
            ++ styleCode "devDependencies"
            ++ " to "
            ++ styleCode "dependencies"
            ++ " in your "
            ++ styleCode "package.json"
            ++ " and run "
            ++ styleCode "npm install"
            ++ "."

      versionMismatchError :: Bool -> SV.Version -> SV.Version -> CommandError
      versionMismatchError isRunByNpmPackage cliVersion installedVersion =
        CommandError "Wasp version mismatch" $
          if isRunByNpmPackage
            then -- \@wasp/cli and \@wasp/wasp differ, e.g. after a partial install.
              "This project's "
                ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
                ++ " doesn't match its Wasp CLI "
                ++ show cliVersion
                ++ ". Run "
                ++ styleCode "npm install"
                ++ " to install them together."
            else -- Nothing handed over to the project's Wasp CLI.
              "This project uses Wasp "
                ++ show installedVersion
                ++ ", but you ran Wasp CLI "
                ++ show cliVersion
                ++ " directly, which can't switch to the project's version. Run the project's Wasp with "
                ++ styleCode "npm run wasp -- <command>"
                ++ ". If you installed Wasp with the installer script, migrate to npm: "
                ++ "https://wasp.sh/docs/guides/legacy/installer#migrate"

-- | Where the user's package.json lists \@wasp/wasp, if anywhere.
data WaspNpmPackageDeclaration
  = -- | Listed in `dependencies`, with this version.
    DeclaredInDependencies String
  | DeclaredInDevDependencies
  | NotDeclared

getWaspNpmPackageDeclaration :: Path' Abs (Dir WaspProjectDir) -> IO (Either String WaspNpmPackageDeclaration)
getWaspNpmPackageDeclaration waspProjectDir =
  findUserPackageJsonFile waspProjectDir >>= \case
    Nothing -> return $ Right NotDeclared
    Just packageJsonFile ->
      fmap getDeclaration <$> P.parsePackageJsonFile packageJsonFile
  where
    getDeclaration packageJson =
      case M.lookup waspNpmPackageName (P.dependencies packageJson) of
        Just version -> DeclaredInDependencies version
        Nothing
          | M.member waspNpmPackageName (P.devDependencies packageJson) -> DeclaredInDevDependencies
          | otherwise -> NotDeclared
