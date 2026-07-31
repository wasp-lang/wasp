module Wasp.Cli.Command.Require.InLockedWaspProject
  ( InLockedWaspProject (InLockedWaspProject),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Project.Common (WaspProjectDir, projectLockFileInWaspProjectDir)

-- | Carrying the acquired 'ProjectLock.ProjectLock' here keeps its underlying
-- handle reachable for as long as the command runs (checked requirements are
-- stored for the command's lifetime), so the OS lock stays held until the
-- process exits. The lock file itself is never removed; see
-- 'ProjectLock.acquireProjectLock'.
data InLockedWaspProject = InLockedWaspProject (Path' Abs (Dir WaspProjectDir)) ProjectLock.ProjectLock deriving (Typeable)

instance Requirable InLockedWaspProject where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    let lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir

    liftIO (ProjectLock.acquireProjectLock lockFilePath) >>= \case
      Right lock -> return $ InLockedWaspProject waspProjectDir lock
      Left (ProjectLock.ProjectLockHeld maybeProcessId) ->
        throwError $
          CommandError "Wasp project is already in use" $
            "Another Wasp command"
              ++ maybe "" (\processId -> " (PID " ++ show processId ++ ")") maybeProcessId
              ++ " is already running for this project. Stop it before running this command."
