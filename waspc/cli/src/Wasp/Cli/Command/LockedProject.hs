module Wasp.Cli.Command.LockedProject
  ( withLockedProject,
  )
where

import Control.Monad.Catch (bracket)
import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import StrongPath ((</>))
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Cli.ProjectLock as ProjectLock

-- | Runs the given action while holding an exclusive lock on the Wasp project
-- the current working directory is part of, so no other Wasp process can work
-- on the project at the same time. Throws a 'CommandError' if another process
-- already holds the lock.
withLockedProject :: Command a -> Command a
withLockedProject action = do
  InWaspProject waspProjectDir <- require

  bracket
    (acquireProjectLockOrThrow $ waspProjectDir </> ProjectLock.projectLockFileInWaspProjectDir)
    (liftIO . ProjectLock.releaseProjectLock)
    (const action)
  where
    acquireProjectLockOrThrow lockFilePath =
      liftIO (ProjectLock.acquireProjectLock lockFilePath) >>= \case
        Right lockFileHandle -> return lockFileHandle
        Left (ProjectLock.ProjectLockHeld maybeProcessId) ->
          throwError $ makeLockedProjectError maybeProcessId

    makeLockedProjectError maybeProcessId =
      CommandError "Wasp project is already in use" $
        unwords $
          catMaybes
            [ Just "Another Wasp command",
              ("(PID " ++) . (++ ")") . show <$> maybeProcessId,
              Just "is already running for this project."
            ]
