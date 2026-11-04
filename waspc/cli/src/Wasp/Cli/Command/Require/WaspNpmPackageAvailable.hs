module Wasp.Cli.Command.Require.WaspNpmPackageAvailable
  ( WaspNpmPackageAvailable (WaspNpmPackageAvailable),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import qualified Data.Map as M
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Project.WaspNpmPackage
  ( NpmPackageVersion,
    doesVersionMatchDeclaredSpecifier,
    getWaspCliNpmPackageVersion,
    isRunByWaspCliNpmPackage,
    tryGettingInstalledWaspNpmPackageVersion,
    waspNpmPackageName,
  )
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
    declaredSpecifier <-
      liftIO (getWaspNpmPackageDeclaration waspProjectDir)
        >>= either (throwError . CommandError "Couldn't read package.json") return
        >>= \case
          DeclaredInDependencies specifier -> return specifier
          DeclaredInDevDependencies -> throwError inDevDependenciesError
          NotDeclared -> throwError $ notADependencyError cliVersion
    liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
      Right installedVersion
        | not (doesVersionMatchDeclaredSpecifier declaredSpecifier installedVersion) ->
            throwError $ missingDepsError $ staleInstallReason declaredSpecifier installedVersion
        | cliVersion /= installedVersion -> do
            isRunByNpmPackage <- liftIO isRunByWaspCliNpmPackage
            throwError $ versionMismatchError isRunByNpmPackage cliVersion installedVersion
        | otherwise -> return WaspNpmPackageAvailable
      Left reason -> throwError $ missingDepsError reason
    where
      missingDepsError reason =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your project dependencies are out of date. Run "
            ++ styleCode "npm install"
            ++ " to fix this.\n"
            ++ "Details: "
            ++ reason

      staleInstallReason :: String -> NpmPackageVersion -> String
      staleInstallReason declaredSpecifier installedVersion =
        "The package.json asks for "
          ++ waspNpmPackageName
          ++ " "
          ++ declaredSpecifier
          ++ ", but node_modules has "
          ++ waspNpmPackageName
          ++ " "
          ++ show installedVersion
          ++ "."

      notADependencyError :: NpmPackageVersion -> CommandError
      notADependencyError cliVersion =
        CommandError
          "Wasp is not a dependency of this project"
          $ "Since Wasp 0.27, a project depends on Wasp through the "
            ++ styleCode waspNpmPackageName
            ++ " npm package. Add "
            ++ styleCode ("\"" ++ waspNpmPackageName ++ "\": \"" ++ show cliVersion ++ "\"")
            ++ " to the "
            ++ styleCode "dependencies"
            ++ " in your "
            ++ styleCode "package.json"
            ++ ", remove "
            ++ styleCode "@wasp.sh/spec"
            ++ " if it's there, and run "
            ++ styleCode "npm install"
            ++ "."

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

      -- The npm Wasp CLI hands over to the project's Wasp CLI, which
      -- \@wasp/wasp pins to its own version. So a mismatch means either that
      -- someone ran the binary directly, without anything to hand over to the
      -- project's Wasp, or that the project's \@wasp/cli and \@wasp/wasp
      -- don't match, e.g. after a partial install.
      versionMismatchError :: Bool -> NpmPackageVersion -> NpmPackageVersion -> CommandError
      versionMismatchError isRunByNpmPackage cliVersion installedVersion =
        CommandError "Wasp version mismatch" $
          if isRunByNpmPackage
            then
              "This project's "
                ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
                ++ " doesn't match its Wasp CLI "
                ++ show cliVersion
                ++ ". Run "
                ++ styleCode "npm install"
                ++ " to install them together."
            else
              "This project uses Wasp "
                ++ show installedVersion
                ++ ", but you ran Wasp CLI "
                ++ show cliVersion
                ++ " directly, which can't switch to the project's version. Run the project's Wasp with "
                ++ styleCode "npm run wasp -- <command>"
                ++ ", or upgrade your global Wasp CLI to the latest version. Read how at https://wasp.sh/docs/quick-start#global-cli."

-- | Where the user's package.json lists \@wasp/wasp, if anywhere.
data WaspNpmPackageDeclaration
  = -- | Listed in `dependencies`, with this specifier.
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
        Just specifier -> DeclaredInDependencies specifier
        Nothing
          | M.member waspNpmPackageName (P.devDependencies packageJson) -> DeclaredInDevDependencies
          | otherwise -> NotDeclared
