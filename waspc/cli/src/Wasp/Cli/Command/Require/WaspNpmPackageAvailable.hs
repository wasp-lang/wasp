module Wasp.Cli.Command.Require.WaspNpmPackageAvailable
  ( WaspNpmPackageAvailable (WaspNpmPackageAvailable),
  )
where

import Control.Applicative ((<|>))
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
import Wasp.Project.WaspNpmPackage (tryGettingInstalledWaspNpmPackageVersion, waspNpmPackageName)
import qualified Wasp.SemanticVersion as SV
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

-- | Require that the project's @wasp/wasp npm package is installed in
-- node_modules and that its version matches this CLI's version.
data WaspNpmPackageAvailable = WaspNpmPackageAvailable deriving (Typeable)

instance Requirable WaspNpmPackageAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Reading the wasp spec runs Node.js (via the FFI), so it requires Node.js
    -- and npm to be present.
    ValidNodeAndNpm <- require
    declaredSpecifier <-
      liftIO (getDeclaredWaspNpmPackageSpecifier waspProjectDir)
        >>= either (throwError . CommandError "Couldn't read package.json") return
    liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
      Right installedVersion
        | installedVersion == WV.waspVersion -> return WaspNpmPackageAvailable
        | declaredSpecifier `elem` [Just (show WV.waspVersion), Just ("^" ++ show WV.waspVersion), Just ("~" ++ show WV.waspVersion)] ->
            throwError $ staleInstallError installedVersion
        | otherwise -> throwError $ versionMismatchError installedVersion
      Left reason ->
        throwError $ case declaredSpecifier of
          Just _ -> missingDepsError reason
          Nothing -> notADependencyError
    where
      missingDepsError reason =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your project dependencies are out of date. Run "
            ++ styleCode "npm install"
            ++ " to fix this.\n"
            ++ "Details: "
            ++ reason

      -- The package.json already asks for this CLI's version, so the user
      -- changed it and did not reinstall yet.
      staleInstallError :: SV.Version -> CommandError
      staleInstallError installedVersion =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your "
            ++ styleCode "package.json"
            ++ " asks for "
            ++ styleCode (waspNpmPackageName ++ " " ++ show WV.waspVersion)
            ++ ", but "
            ++ show installedVersion
            ++ " is installed. Run "
            ++ styleCode "npm install"
            ++ " to fix this."

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
            ++ ", remove the "
            ++ styleCode "@wasp/spec"
            ++ " (or "
            ++ styleCode "@wasp.sh/spec"
            ++ ") dependency, and run "
            ++ styleCode "npm install"
            ++ "."

      versionMismatchError :: SV.Version -> CommandError
      versionMismatchError installedVersion =
        CommandError
          "Wasp version mismatch"
          $ "This project has "
            ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
            ++ " installed, but you are running Wasp CLI "
            ++ show WV.waspVersion
            ++ ". Run Wasp through the project (e.g. "
            ++ styleCode "npm run wasp start"
            ++ "), or set "
            ++ styleCode waspNpmPackageName
            ++ " in your "
            ++ styleCode "package.json"
            ++ " to "
            ++ show WV.waspVersion
            ++ " and run "
            ++ styleCode "npm install"
            ++ "."

-- | What the user's package.json asks for @wasp/wasp, if it depends on it at all.
getDeclaredWaspNpmPackageSpecifier :: Path' Abs (Dir WaspProjectDir) -> IO (Either String (Maybe String))
getDeclaredWaspNpmPackageSpecifier waspProjectDir =
  findUserPackageJsonFile waspProjectDir >>= \case
    Nothing -> return $ Right Nothing
    Just packageJsonFile ->
      fmap declaredSpecifier <$> P.parsePackageJsonFile packageJsonFile
  where
    declaredSpecifier packageJson =
      M.lookup waspNpmPackageName (P.dependencies packageJson)
        <|> M.lookup waspNpmPackageName (P.devDependencies packageJson)
