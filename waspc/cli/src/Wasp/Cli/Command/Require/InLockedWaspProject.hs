module Wasp.Cli.Command.Require.InLockedWaspProject
  ( InLockedWaspProject (InLockedWaspProject),
  )
where

import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Resource (allocate)
import Data.Data (Typeable)
import Data.Either (isRight)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Cli.ProjectLock as ProjectLock
import Wasp.Project.Common (WaspProjectDir, projectLockFileInWaspProjectDir)

newtype InLockedWaspProject = InLockedWaspProject (Path' Abs (Dir WaspProjectDir)) deriving (Typeable)

instance Requirable InLockedWaspProject where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    let lockFilePath = waspProjectDir </> projectLockFileInWaspProjectDir

    (_, lockResult) <-
      allocate
        (ProjectLock.acquireProjectLock lockFilePath)
        (\lockResult -> when (isRight lockResult) $ ProjectLock.releaseProjectLock lockFilePath)

    case lockResult of
      Right _ -> return $ InLockedWaspProject waspProjectDir
      Left lockError -> throwError $ commandError lockFilePath lockError
    where
      commandError _ (ProjectLock.ProjectLockHeld processId) =
        CommandError "Wasp project is already in use" $
          "Another Wasp command (PID "
            ++ show processId
            ++ ") is already running for this project. Stop it before running this command."
      commandError lockFilePath (ProjectLock.ProjectLockMalformed _) =
        CommandError "Wasp project is already in use" $
          "Wasp couldn't determine which process owns the lock at "
            ++ SP.fromAbsFile lockFilePath
            ++ ". If no other Wasp command is running, remove the lock file and try again."
      commandError lockFilePath (ProjectLock.ProjectLockOwnerCheckFailed processId errorMessage) =
        CommandError "Wasp project is already in use" $
          unlines
            [ "Wasp couldn't check whether the command with PID "
                ++ show processId
                ++ " is still running. To avoid corrupting the project, the lock at "
                ++ SP.fromAbsFile lockFilePath
                ++ " was left in place.",
              errorMessage,
              "\nIf no other Wasp command is running, remove the lock file and try again."
            ]
