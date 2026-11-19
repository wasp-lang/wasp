module Wasp.Cli.Command.Require.ValidNodeAndPackageManager
  ( ValidNodeAndPackageManager (ValidNodeAndPackageManager),
  )
where

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Wasp.Cli.Command (Command, CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.PackageManager (PackageManager (..), defaultPackageManager, detectPackageManagerOrDefault)

-- | Require that the Node.js and the package manager installed on the user's
-- machine meet Wasp's version requirements. Any command that runs Node.js
-- (compilation, running the generated app, Prisma, the deploy/studio packages,
-- ...) should require this so the user gets a clear error early on.
data ValidNodeAndPackageManager = ValidNodeAndPackageManager deriving (Typeable)

instance Requirable ValidNodeAndPackageManager where
  checkRequirement = do
    packageManager <- findPackageManager
    liftIO (runExceptT $ mapM_ ExceptT $ getVersionChecks packageManager) >>= \case
      Left errorMsg ->
        throwError $ CommandError "Node.js or package manager requirement not met" errorMsg
      Right () -> return ValidNodeAndPackageManager
    where
      getVersionChecks = \case
        Npm -> [NodeVersion.checkUserNodeMeetsWaspRequirements, NodeVersion.checkUserNpmMeetsWaspRequirements]
        -- Wasp has no known minimum version of these.
        Pnpm -> [NodeVersion.checkUserNodeMeetsWaspRequirements]
        Bun -> [NodeVersion.checkUserNodeMeetsWaspRequirements]

-- | A command can run outside of a project, e.g. `wasp new`, where there is no
-- package manager to find yet.
findPackageManager :: Command PackageManager
findPackageManager =
  (require >>= \(InWaspProject waspProjectDir) -> liftIO $ detectPackageManagerOrDefault waspProjectDir)
    `catchError` const (return defaultPackageManager)
