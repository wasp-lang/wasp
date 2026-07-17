{-# LANGUAGE CPP #-}

module Wasp.Cli.ProjectLock
  ( WaspProcessId,
    ProjectLockError (..),
    acquireProjectLock,
    releaseProjectLock,
    projectLockFilePath,
    getCurrentWaspProcessId,
  )
where

import Control.Exception (IOException, displayException, try)
import Data.Text (Text, strip)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as Directory
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)
import Wasp.Project.Common (DotWaspDir, WaspProjectDir, dotWaspDirInWaspProjectDir)
import Wasp.Util.IO (deleteFileIfExists)

#ifdef mingw32_HOST_OS
import qualified System.Win32.Process as Process
#else
import qualified System.Posix.Process as Process
import qualified System.Posix.Signals as Signals
import qualified System.Posix.Types as Posix
#endif

newtype WaspProcessId = WaspProcessId Integer
  deriving (Eq)

instance Show WaspProcessId where
  show (WaspProcessId processId) = show processId

data ProjectLockError
  = ProjectLockHeld WaspProcessId
  | ProjectLockMalformed
  | ProjectLockOwnerCheckFailed WaspProcessId String
  deriving (Eq, Show)

acquireProjectLock ::
  Path' Abs (Dir WaspProjectDir) ->
  IO (Either ProjectLockError WaspProcessId)
acquireProjectLock waspProjectDir = do
  Directory.createDirectoryIfMissing True $ SP.fromAbsDir dotWaspDir
  lockExists <- Directory.doesFileExist lockFile
  if lockExists
    then inspectExistingLock
    else acquire
  where
    dotWaspDir = waspProjectDir </> dotWaspDirInWaspProjectDir
    lockFilePath = projectLockFilePath waspProjectDir
    lockFile = SP.fromAbsFile lockFilePath

    acquire = do
      processId <- getCurrentWaspProcessId
      Text.IO.writeFile lockFile $ processIdToText processId
      return $ Right processId

    inspectExistingLock = do
      processIdOrError <- readLockOwner lockFilePath
      case processIdOrError of
        Left ReadLockFileMissing -> acquireProjectLock waspProjectDir
        Left ReadLockFileMalformed -> return $ Left ProjectLockMalformed
        Right processId ->
          isProcessAlive processId >>= \case
            Left errorMessage -> return $ Left $ ProjectLockOwnerCheckFailed processId errorMessage
            Right True -> return $ Left $ ProjectLockHeld processId
            Right False -> do
              deleteFileIfExists lockFilePath
              acquireProjectLock waspProjectDir

releaseProjectLock ::
  Path' Abs (Dir WaspProjectDir) ->
  IO ()
releaseProjectLock = deleteFileIfExists . projectLockFilePath

projectLockFilePath ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' Abs File'
projectLockFilePath waspProjectDir =
  waspProjectDir </> dotWaspDirInWaspProjectDir </> projectLockFileInDotWaspDir

projectLockFileInDotWaspDir :: Path' (Rel DotWaspDir) File'
projectLockFileInDotWaspDir = [relfile|.lock|]

data ReadLockFileError = ReadLockFileMissing | ReadLockFileMalformed

readLockOwner :: Path' Abs File' -> IO (Either ReadLockFileError WaspProcessId)
readLockOwner lockFilePath = do
  contentsOrError <- try $ Text.IO.readFile $ SP.fromAbsFile lockFilePath
  case contentsOrError of
    Left ioException
      | isDoesNotExistError ioException -> return $ Left ReadLockFileMissing
      | otherwise -> ioError ioException
    Right contents ->
      return $
        maybe
          (Left ReadLockFileMalformed)
          Right
          (parseProcessId $ Text.unpack $ strip contents)

processIdToText :: WaspProcessId -> Text
processIdToText (WaspProcessId processId) = Text.pack $ show processId

parseProcessId :: String -> Maybe WaspProcessId
parseProcessId processIdText = do
  processId <- readMaybe processIdText
  if isValidProcessId processId
    then Just $ WaspProcessId processId
    else Nothing

getCurrentWaspProcessId :: IO WaspProcessId
#ifdef mingw32_HOST_OS
getCurrentWaspProcessId =
  WaspProcessId . fromIntegral <$> Process.getCurrentProcessId
#else
getCurrentWaspProcessId =
  WaspProcessId . fromIntegral <$> Process.getProcessID
#endif

isProcessAlive :: WaspProcessId -> IO (Either String Bool)
#ifdef mingw32_HOST_OS
isProcessAlive (WaspProcessId processId) = do
  processesOrError <-
    try $
      Process.withTh32Snap Process.tH32CS_SNAPPROCESS Nothing Process.th32SnapEnumProcesses
  return $ case processesOrError of
    Left ioException -> Left $ displayException (ioException :: IOException)
    Right processes ->
      Right $
        any
          (\(candidateProcessId, _, _, _, _) -> fromIntegral candidateProcessId == processId)
          processes
#else
isProcessAlive (WaspProcessId processId) = do
  signalResult <-
    try $
      Signals.signalProcess Signals.nullSignal (fromIntegral processId :: Posix.ProcessID)
  return $ case signalResult of
    Right () -> Right True
    Left ioException
      | isDoesNotExistError ioException -> Right False
      | otherwise -> Left $ displayException (ioException :: IOException)
#endif

isValidProcessId :: Integer -> Bool
isValidProcessId processId =
  processId > 0 && processId <= maxNativeProcessId
  where
#ifdef mingw32_HOST_OS
    maxNativeProcessId = fromIntegral (maxBound :: Process.ProcessId)
#else
    maxNativeProcessId = fromIntegral (maxBound :: Posix.ProcessID)
#endif
