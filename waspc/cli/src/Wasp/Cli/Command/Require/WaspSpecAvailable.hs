module Wasp.Cli.Command.Require.WaspSpecAvailable
  ( WaspSpecAvailable (WaspSpecAvailable),
    isWaspSpecAvailable,
  )
where

import Control.Monad (unless)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

-- | Require that the @wasp.sh/spec package is available in node_modules and that
-- its version matches this CLI's version.
data WaspSpecAvailable = WaspSpecAvailable deriving (Typeable)

instance Requirable WaspSpecAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Reading the wasp spec runs Node.js (via the FFI), so it requires Node.js
    -- and npm to be present.
    ValidNodeAndNpm <- require
    isAvailable <- liftIO $ isWaspSpecAvailable waspProjectDir
    unless isAvailable $ throwError missingDepsError
    return WaspSpecAvailable
    where
      missingDepsError =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your project dependencies are out of date. Run " ++ styleCode "wasp install" ++ " to fix this."

isWaspSpecAvailable :: Path' Abs (Dir WaspProjectDir) -> IO Bool
isWaspSpecAvailable waspProjectDir =
  tryGettingInstalledPackageVersion waspProjectDir WaspSpecPackage >>= \case
    Left _ -> return False
    Right installedWaspSpecVersion -> return $ installedWaspSpecVersion == WV.waspVersion
