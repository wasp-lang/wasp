module Wasp.Cli.Command.Require.WaspNpmPackageAvailable
  ( WaspNpmPackageAvailable (WaspNpmPackageAvailable),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Data.Either.Extra (eitherToMaybe)
import qualified Data.Map as M
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Project.WaspNpmPackage (tryGettingInstalledWaspNpmPackageVersion, waspNpmPackageName)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

-- | Require that the project's \@wasp/wasp npm package is installed in
-- node_modules and that it and this CLI satisfy the version the project asks for.
data WaspNpmPackageAvailable = WaspNpmPackageAvailable deriving (Typeable)

instance Requirable WaspNpmPackageAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Commands that require this read the Wasp Spec, which runs Node.js, so
    -- they also need Node.js and npm.
    ValidNodeAndNpm <- require
    declaredSpecifier <-
      liftIO (getDeclaredWaspNpmPackageSpecifier waspProjectDir)
        >>= either (throwError . CommandError "Couldn't read package.json") return
        >>= maybe (throwError notADependencyError) return
    let declaredRange = eitherToMaybe $ SV.parseRange declaredSpecifier
    liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
      Right installedVersion
        | not (isVersionInDeclaredRange declaredRange installedVersion) ->
            throwError $ missingDepsError $ staleInstallReason declaredSpecifier installedVersion
        | not (isCliCompatibleWithInstalledWaspNpmPackage declaredRange installedVersion) ->
            throwError $ versionMismatchError declaredSpecifier installedVersion
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

      staleInstallReason :: String -> SV.Version -> String
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

      notADependencyError =
        CommandError
          "Wasp is not a dependency of this project"
          $ "Since Wasp 0.27, a project depends on Wasp through the "
            ++ styleCode waspNpmPackageName
            ++ " npm package. Add "
            ++ styleCode ("\"" ++ waspNpmPackageName ++ "\": \"" ++ show WV.waspVersion ++ "\"")
            ++ " to the "
            ++ styleCode "dependencies"
            ++ " in your "
            ++ styleCode "package.json"
            ++ ", remove any "
            ++ styleCode "@wasp/spec"
            ++ " or "
            ++ styleCode "@wasp.sh/spec"
            ++ " dependency, and run "
            ++ styleCode "npm install"
            ++ "."

      versionMismatchError :: String -> SV.Version -> CommandError
      versionMismatchError declaredSpecifier installedVersion =
        CommandError
          "Wasp version mismatch"
          $ "This project has "
            ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
            ++ " installed and asks for "
            ++ styleCode (waspNpmPackageName ++ " " ++ declaredSpecifier)
            ++ ", but you are running Wasp CLI "
            ++ show WV.waspVersion
            ++ ". Run a Wasp CLI version that the project asks for, or set "
            ++ styleCode waspNpmPackageName
            ++ " in your "
            ++ styleCode "package.json"
            ++ " to "
            ++ show WV.waspVersion
            ++ " and run "
            ++ styleCode "npm install"
            ++ "."

-- | Whether the version satisfies the range the project's package.json
-- declares for \@wasp/wasp. Specifiers that aren't version ranges, e.g. `file:`
-- tarballs, don't constrain the version.
isVersionInDeclaredRange :: Maybe SV.Range -> SV.Version -> Bool
isVersionInDeclaredRange declaredRange version =
  maybe True (SV.isVersionInRange version) declaredRange

-- | This CLI works with the installed \@wasp/wasp when the CLI's version
-- satisfies the range the project's package.json declares. Without a declared
-- range, the CLI's version must match the installed version exactly.
isCliCompatibleWithInstalledWaspNpmPackage :: Maybe SV.Range -> SV.Version -> Bool
isCliCompatibleWithInstalledWaspNpmPackage declaredRange installedVersion =
  case declaredRange of
    Just range -> SV.isVersionInRange WV.waspVersion range
    Nothing -> installedVersion == WV.waspVersion

-- | What the user's package.json asks for \@wasp/wasp in its `dependencies`,
-- if anything.
getDeclaredWaspNpmPackageSpecifier :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Maybe String))
getDeclaredWaspNpmPackageSpecifier waspProjectDir =
  findUserPackageJsonFile waspProjectDir >>= \case
    Nothing -> return $ Right Nothing
    Just packageJsonFile ->
      fmap declaredSpecifier <$> P.parsePackageJsonFile packageJsonFile
  where
    declaredSpecifier packageJson =
      M.lookup waspNpmPackageName (P.dependencies packageJson)
