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
    liftIO (tryGettingInstalledWaspNpmPackageVersion waspProjectDir) >>= \case
      Right installedVersion
        | installedVersion == WV.waspVersion -> return WaspNpmPackageAvailable
        | otherwise -> throwError $ versionMismatchError installedVersion
      Left _ -> do
        isDeclared <- liftIO $ userPackageJsonDependsOnWaspNpmPackage waspProjectDir
        throwError $ if isDeclared then missingDepsError else notADependencyError
    where
      missingDepsError =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your project dependencies are out of date. Run " ++ styleCode "npm install" ++ " to fix this."

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
          $ "This project depends on "
            ++ styleCode (waspNpmPackageName ++ " " ++ show installedVersion)
            ++ ", but you are running Wasp CLI "
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

userPackageJsonDependsOnWaspNpmPackage :: Path' Abs (Dir WaspProjectDir) -> IO Bool
userPackageJsonDependsOnWaspNpmPackage waspProjectDir =
  findUserPackageJsonFile waspProjectDir >>= \case
    Nothing -> return False
    Just packageJsonFile ->
      either (const False) dependsOnWaspNpmPackage <$> P.parsePackageJsonFile packageJsonFile
  where
    dependsOnWaspNpmPackage packageJson =
      any
        (M.member waspNpmPackageName)
        [P.dependencies packageJson, P.devDependencies packageJson]
