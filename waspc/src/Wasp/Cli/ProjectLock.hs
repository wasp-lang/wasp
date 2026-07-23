module Wasp.Cli.ProjectLock
  ( WaspProcessId,
    ProjectLockError (..),
    acquireProjectLock,
    releaseProjectLock,
  )
where

import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import StrongPath (Abs, File, Path')
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO.Error (isDoesNotExistError)
import Text.Read (readEither)
import Wasp.Cli.ProjectLock.System (getCurrentWaspProcessId, isProcessAlive)
import Wasp.Project.Common
  ( WaspProjectLockfile,
  )
import Wasp.Util (trim)
import qualified Wasp.Util.IO as Wasp.IO
import Prelude hiding (writeFile)

type WaspProcessId = Integer

data ProjectLockError
  = ProjectLockHeld WaspProcessId
  | ProjectLockMalformed String
  | ProjectLockOwnerCheckFailed WaspProcessId String
  deriving (Eq, Show)

acquireProjectLock :: Path' Abs (File WaspProjectLockfile) -> IO (Either ProjectLockError ())
acquireProjectLock lockFilePath = runExceptT $ do
  lockExists <- liftIO $ Wasp.IO.doesFileExist lockFilePath
  when lockExists $ do
    lockOwner <- readLockOwner lockFilePath
    maybe (pure ()) assertProcessIsDead lockOwner
  acquire
  where
    assertProcessIsDead processId = do
      processIsAlive <-
        withExceptT (ProjectLockOwnerCheckFailed processId) $
          ExceptT (isProcessAlive processId)

      when processIsAlive $
        throwError (ProjectLockHeld processId)

    acquire = liftIO $ do
      Directory.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent lockFilePath
      processId <- getCurrentWaspProcessId
      Wasp.IO.writeFile lockFilePath (show processId)

releaseProjectLock :: Path' Abs (File WaspProjectLockfile) -> IO ()
releaseProjectLock = Wasp.IO.deleteFileIfExists

readLockOwner :: Path' Abs (File WaspProjectLockfile) -> ExceptT ProjectLockError IO (Maybe WaspProcessId)
readLockOwner lockFilePath = ExceptT $ do
  try (Wasp.IO.readFile lockFilePath) >>= \case
    Left ioException
      | isDoesNotExistError ioException -> pure $ Right Nothing
      | otherwise -> (liftIO $ ioError ioException)
    Right contents ->
      return $
        bimap ProjectLockMalformed Just $
          readEither (trim contents)
