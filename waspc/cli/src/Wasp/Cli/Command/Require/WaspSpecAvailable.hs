module Wasp.Cli.Command.Require.WaspSpecAvailable
  ( WaspSpecAvailable (WaspSpecAvailable),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Project.Install (isInstalledWaspSpecMatchingCliVersion)
import Wasp.Util.Terminal (styleCode)

-- | Require that the @wasp.sh/spec package is available in node_modules and that
-- its version matches this CLI's version.
data WaspSpecAvailable = WaspSpecAvailable deriving (Typeable)

instance Requirable WaspSpecAvailable where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    -- Reading the wasp spec runs Node.js (via the FFI), so it requires Node.js
    -- and npm to be present.
    ValidNodeAndNpm <- require
    isCurrent <- liftIO $ isInstalledWaspSpecMatchingCliVersion waspProjectDir
    if isCurrent then return () else throwError missingDepsError
    return WaspSpecAvailable
    where
      missingDepsError =
        CommandError
          "Missing or stale dependencies in project"
          $ "Your project dependencies are out of date. Run " ++ styleCode "wasp install" ++ " to fix this."
