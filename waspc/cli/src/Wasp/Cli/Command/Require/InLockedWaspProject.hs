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

newtype InLockedWaspProject = InLockedWaspProject (Path' Abs (Dir WaspProjectDir)) deriving (Typeable)

instance Requirable InLockedWaspProject where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    let lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir

    liftIO (ProjectLock.acquireProjectLock lockFilePath) >>= \case
      Right _ -> return $ InLockedWaspProject waspProjectDir
      Left (ProjectLock.ProjectLockHeld maybeProcessId) ->
        throwError $
          CommandError "Wasp project is already in use" $
            "Another Wasp command"
              ++ maybe "" (\processId -> " (PID " ++ show processId ++ ")") maybeProcessId
              ++ " is already running for this project. Stop it before running this command."
