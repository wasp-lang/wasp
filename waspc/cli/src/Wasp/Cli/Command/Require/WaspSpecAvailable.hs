module Wasp.Cli.Command.Require.WaspSpecAvailable
  ( WaspSpecAvailable (WaspSpecAvailable),
    ensureWaspSpecAvailable,
  )
where

import Control.Monad (unless)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Install (LockfileHandling (KeepExistingLockfile), installIO)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Message as Msg
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), getInstallablePackageName, tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

-- | Require that the @wasp.sh/spec package is available in node_modules and that
-- its version matches this CLI's version.
--
-- Commands that hold the project lock should call 'ensureWaspSpecAvailable'
-- instead, which installs the dependencies rather than failing.
data WaspSpecAvailable = WaspSpecAvailable deriving (Typeable)

instance Requirable WaspSpecAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Reading the wasp spec runs Node.js (via the FFI), so it requires Node.js
    -- and npm to be present.
    ValidNodeAndNpm <- require
    isAvailable <- liftIO $ isWaspSpecAvailable waspProjectDir
    unless isAvailable $ throwError missingOrStaleDepsError
    return WaspSpecAvailable

-- | Same check as the 'WaspSpecAvailable' requirement, but when it fails, it
-- installs the project's dependencies instead of failing right away.
--
-- Unlike `wasp install`, it keeps an existing `package-lock.json` untouched
-- (see 'KeepExistingLockfile').
--
-- Installing changes `node_modules` and `.wasp/spec`, so only call this while
-- holding the project lock.
ensureWaspSpecAvailable :: Path' Abs (Dir WaspProjectDir) -> Command ()
ensureWaspSpecAvailable waspProjectDir = do
  -- Reading the wasp spec runs Node.js and installing runs npm.
  ValidNodeAndNpm <- require
  isAvailable <- liftIO $ isWaspSpecAvailable waspProjectDir
  unless isAvailable $ do
    cliSendMessageC $ Msg.Start "Installing missing or outdated project dependencies..."
    liftIO (installIO KeepExistingLockfile waspProjectDir)
      >>= either (throwError . CommandError "Couldn't install npm dependencies") return
    isAvailableAfterInstall <- liftIO $ isWaspSpecAvailable waspProjectDir
    unless isAvailableAfterInstall $ throwError waspSpecMissingAfterInstallError
    cliSendMessageC $ Msg.Success "Installed project dependencies."

isWaspSpecAvailable :: Path' Abs (Dir WaspProjectDir) -> IO Bool
isWaspSpecAvailable waspProjectDir =
  tryGettingInstalledPackageVersion waspProjectDir WaspSpecPackage >>= \case
    Left _ -> return False
    Right installedWaspSpecVersion -> return $ installedWaspSpecVersion == WV.waspVersion

missingOrStaleDepsError :: CommandError
missingOrStaleDepsError =
  CommandError
    "Missing or stale dependencies in project"
    $ "Your project dependencies are out of date. Run " ++ styleCode "wasp install" ++ " to fix this."

waspSpecMissingAfterInstallError :: CommandError
waspSpecMissingAfterInstallError =
  CommandError
    "Missing or stale dependencies in project"
    $ "npm install finished, but "
      ++ styleCode ("node_modules/" ++ waspSpecPackageName ++ "/package.json")
      ++ " is still missing or its version isn't "
      ++ show WV.waspVersion
      ++ " (the version of this Wasp CLI). Check that your package.json lists "
      ++ styleCode ("\"" ++ waspSpecPackageName ++ "\": \"file:.wasp/spec\"")
      ++ " in its devDependencies."
  where
    waspSpecPackageName = getInstallablePackageName WaspSpecPackage
