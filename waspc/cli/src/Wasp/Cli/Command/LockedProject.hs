module Wasp.Cli.Command.LockedProject
  ( withLockedProject,
  )
where

import Control.Monad.Catch (bracket)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Lukko
import StrongPath ((</>))
import System.IO (hClose)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Project.Common (projectLockFileInWaspProjectDir)

-- | Runs the given action while holding an exclusive lock on the Wasp project
-- the current working directory is part of, so no other Wasp process can work
-- on the project at the same time. Throws a 'CommandError' if another process
-- already holds the lock.
withLockedProject :: Command a -> Command a
withLockedProject action = do
  InWaspProject waspProjectDir <- require
  let lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir
  bracket
    (acquireProjectLockOrThrow lockFilePath)
    unlockAndClose
    (const action)
  where
    acquireProjectLockOrThrow lockFilePath =
      liftIO (ProjectLock.acquireProjectLock lockFilePath) >>= \case
        Right lockFileHandle -> return lockFileHandle
        Left (ProjectLock.ProjectLockHeld maybeProcessId) ->
          throwError $
            CommandError "Wasp project is already in use" $
              "Another Wasp command"
                ++ maybe "" (\processId -> " (PID " ++ show processId ++ ")") maybeProcessId
                ++ " is already running for this project. Stop it before running this command."

    -- The lock file itself stays behind; see 'ProjectLock.acquireProjectLock'
    -- for why it must not be deleted.
    unlockAndClose lockFileHandle = liftIO $ do
      Lukko.hUnlock lockFileHandle
      hClose lockFileHandle
