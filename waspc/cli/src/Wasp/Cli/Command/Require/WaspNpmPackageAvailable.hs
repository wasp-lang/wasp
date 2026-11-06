module Wasp.Cli.Command.Require.WaspNpmPackageAvailable
  ( WaspNpmPackageAvailable (WaspNpmPackageAvailable),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import qualified Data.Map as M
import StrongPath (Abs, Dir, Path', fromAbsDir)
import System.IO (hPutStrLn, stderr)
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Terminal (asWaspWarningMessage)
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.NpmPackageNames (waspCliNpmPackageName, waspNpmPackageName)
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
import qualified Wasp.Util.Terminal as Term

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
    declaredVersion <-
      liftIO (getWaspNpmPackageDeclaration waspProjectDir)
        >>= either (throwError . CommandError "Couldn't read package.json") return
        >>= \case
          DeclaredInDependencies version -> return version
          DeclaredInDevDependencies version -> do
            liftIO $ warnOnStderr inDevDependenciesWarningTitle inDevDependenciesWarning
            return version
          NotDeclared -> throwError $ notADependencyError cliVersion
    liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
      Right installedVersion
        | not (doesVersionMatchDeclaredVersion declaredVersion installedVersion) ->
            throwError
              $ outOfDateError
              $ "The package.json asks for "
                ++ styleCode (waspNpmPackageName ++ " " ++ declaredVersion)
                ++ ", but node_modules has "
                ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
                ++ "."
        | cliVersion /= installedVersion -> do
            isRunByNpmPackage <- liftIO isRunByWaspCliNpmPackage
            throwError $
              if isRunByNpmPackage
                then -- E.g. after a partial install.
                  outOfDateError $
                    "node_modules has "
                      ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
                      ++ ", but "
                      ++ styleCode (waspCliNpmPackageName ++ " " ++ show cliVersion)
                      ++ "."
                else directRunError cliVersion installedVersion
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

      outOfDateError :: String -> CommandError
      outOfDateError reason =
        CommandError "The installed Wasp is out of date" $
          reason ++ " Run " ++ styleCode "npm install" ++ " to update it."

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

      inDevDependenciesWarningTitle = "Wasp is a dev dependency of this project"
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

      -- Only the \@wasp/cli npm package hands over to the project's Wasp.
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

-- | Prints to stderr, so a warning doesn't break the JSON that commands like
-- `wasp show spec --json` print to stdout.
warnOnStderr :: String -> String -> IO ()
warnOnStderr title body =
  hPutStrLn stderr $ Term.applyStyles [Term.Magenta] $ asWaspWarningMessage (title ++ ":") ++ body

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
      fmap getDeclaration <$> P.parsePackageJsonFile packageJsonFile
  where
    getDeclaration packageJson =
      case ( M.lookup waspNpmPackageName (P.dependencies packageJson),
             M.lookup waspNpmPackageName (P.devDependencies packageJson)
           ) of
        (Just version, _) -> DeclaredInDependencies version
        (Nothing, Just version) -> DeclaredInDevDependencies version
        (Nothing, Nothing) -> NotDeclared
