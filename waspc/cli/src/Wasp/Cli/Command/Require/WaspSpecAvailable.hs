module Wasp.Cli.Command.Require.WaspSpecAvailable
  ( WaspSpecAvailable (WaspSpecAvailable),
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
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.PackageJson (findUserPackageJsonFile)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

-- | Require that the @wasp/spec package is available in node_modules and that
-- its version matches this CLI's version.
data WaspSpecAvailable = WaspSpecAvailable deriving (Typeable)

instance Requirable WaspSpecAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Reading the wasp spec runs Node.js (via the FFI), so it requires Node.js
    -- and npm to be present.
    ValidNodeAndNpm <- require
    ensureInstalledWaspSpecMatchesCliVersion waspProjectDir
    return WaspSpecAvailable
    where
      ensureInstalledWaspSpecMatchesCliVersion waspProjectDir =
        liftIO (tryGettingInstalledPackageVersion waspProjectDir WaspSpecPackage) >>= \case
          Right installedWaspSpecVersion
            | installedWaspSpecVersion == WV.waspVersion -> return ()
          _ -> do
            dependsOnLegacyWaspSpec <- liftIO $ userPackageJsonDependsOnLegacyWaspSpec waspProjectDir
            throwError $ if dependsOnLegacyWaspSpec then legacyWaspSpecError else missingDepsError
      legacyWaspSpecError =
        CommandError
          "Outdated Wasp spec package"
          $ "Wasp's spec package is now called "
            ++ styleCode "@wasp/spec"
            ++ ". In your "
            ++ styleCode "package.json"
            ++ ", rename the "
            ++ styleCode legacyWaspSpecPackageName
            ++ " dependency to "
            ++ styleCode "@wasp/spec"
            ++ " and update your imports from "
            ++ styleCode legacyWaspSpecPackageName
            ++ ". Then run "
            ++ styleCode "wasp install"
            ++ "."
      missingDepsError =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your project dependencies are out of date. Run " ++ styleCode "wasp install" ++ " to fix this."

legacyWaspSpecPackageName :: P.PackageName
legacyWaspSpecPackageName = "@wasp.sh/spec"

-- | Projects created before Wasp 0.27 depend on @wasp.sh/spec, the old name of
-- @wasp/spec. `wasp install` can't fix that on its own, so we point users to
-- the migration step instead of suggesting `wasp install` forever.
userPackageJsonDependsOnLegacyWaspSpec :: Path' Abs (Dir WaspProjectDir) -> IO Bool
userPackageJsonDependsOnLegacyWaspSpec waspProjectDir =
  findUserPackageJsonFile waspProjectDir >>= \case
    Nothing -> return False
    Just packageJsonFile ->
      either (const False) dependsOnLegacyWaspSpec <$> P.parsePackageJsonFile packageJsonFile
  where
    dependsOnLegacyWaspSpec packageJson =
      any
        (M.member legacyWaspSpecPackageName)
        [P.dependencies packageJson, P.devDependencies packageJson]
