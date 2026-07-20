module Wasp.Cli.Command.Require.ValidNodeAndNpm
  ( ValidNodeAndNpm (ValidNodeAndNpm),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement))
import qualified Wasp.Node.Version as NodeVersion

-- | Require that the Node.js and npm installed on the user's machine meet
-- Wasp's version requirements. Any command that runs Node.js (compilation,
-- running the generated app, npm install, Prisma, the deploy/studio packages,
-- ...) should require this so the user gets a clear error early on.
data ValidNodeAndNpm = ValidNodeAndNpm deriving (Typeable)

instance Requirable ValidNodeAndNpm where
  checkRequirement =
    liftIO NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
      NodeVersion.VersionCheckFail errorMsg ->
        throwError $ CommandError "Node/NPM requirement not met" errorMsg
      NodeVersion.VersionCheckSuccess -> return ValidNodeAndNpm
