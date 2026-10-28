-- | File system steps. Relative paths are resolved against the working directory.
module E2e.Files
  ( writeToFile,
    appendToFile,
    replaceInFile,
    readFromFile,
    copyFile,
    deleteFile,
    deleteDirectory,
    waitForFile,
    copyGitTrackedDir,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import E2e.Process (command, run, stdout)
import E2e.TestM (RunConfig (..), TestEnv (..), TestM, failTest, inDirectory, resolvePath)
import qualified StrongPath as SP
import System.Directory
  ( createDirectoryIfMissing,
    createFileLink,
    doesFileExist,
    doesPathExist,
    getSymbolicLinkTarget,
    pathIsSymbolicLink,
    removeDirectoryRecursive,
    removeFile,
  )
import qualified System.Directory as Directory
import System.FilePath (makeRelative, takeDirectory, (</>))

-- | Creates or overwrites the file, along with any missing parent directories.
writeToFile :: FilePath -> T.Text -> TestM ()
writeToFile path content = do
  absPath <- resolvePath path
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory absPath)
    BS.writeFile absPath (TE.encodeUtf8 content)

-- | Appends the content and a newline to the file.
appendToFile :: FilePath -> T.Text -> TestM ()
appendToFile path content = do
  absPath <- resolvePath path
  liftIO $ BS.appendFile absPath (TE.encodeUtf8 $ content <> "\n")

-- | Replaces all occurrences of the text. Fails if the text isn't in the file.
replaceInFile :: FilePath -> T.Text -> T.Text -> TestM ()
replaceInFile path oldText newText = do
  content <- readFromFile path
  unless (oldText `T.isInfixOf` content)
    $ failTest
    $ "Expected " ++ path ++ " to contain " ++ show oldText ++ "."
  writeToFile path (T.replace oldText newText content)

readFromFile :: FilePath -> TestM T.Text
readFromFile path = do
  absPath <- resolvePath path
  liftIO $ TE.decodeUtf8 <$> BS.readFile absPath

copyFile :: FilePath -> FilePath -> TestM ()
copyFile srcPath destPath = do
  absSrcPath <- resolvePath srcPath
  absDestPath <- resolvePath destPath
  liftIO $ Directory.copyFile absSrcPath absDestPath

deleteFile :: FilePath -> TestM ()
deleteFile path = resolvePath path >>= liftIO . removeFile

-- | Deletes the directory with all of its contents, if it exists.
deleteDirectory :: FilePath -> TestM ()
deleteDirectory path = do
  absPath <- resolvePath path
  liftIO $ do
    exists <- doesPathExist absPath
    when exists $ removeDirectoryRecursive absPath

-- | Waits until some process creates the file.
waitForFile :: FilePath -> Int -> TestM ()
waitForFile path seconds = do
  absPath <- resolvePath path
  deadline <- liftIO $ addUTCTime (fromIntegral seconds) <$> getCurrentTime
  let poll = do
        exists <- liftIO $ doesFileExist absPath
        now <- liftIO getCurrentTime
        unless exists $
          if now > deadline
            then failTest $ "File " ++ path ++ " wasn't created within " ++ show seconds ++ "s."
            else liftIO (threadDelay 200000) >> poll
  poll

-- | Copies the git-tracked files of a directory in the Wasp repo into the destination directory.
-- Ignored and untracked files (e.g. @node_modules@, build outputs) are left out.
copyGitTrackedDir :: FilePath -> FilePath -> TestM ()
copyGitTrackedDir srcDirInGitRoot destDir = do
  gitRootDir <- asks (SP.fromAbsDir . (.runConfig.gitRootDir))
  absDestDir <- resolvePath destDir
  trackedFiles <-
    map T.unpack . filter (not . T.null) . T.splitOn "\0" . stdout
      <$> inDirectory gitRootDir (run $ command "git" ["ls-files", "-z", "--", srcDirInGitRoot])
  liftIO $
    forM_ trackedFiles $ \fileInGitRoot -> do
      let srcFile = gitRootDir </> fileInGitRoot
          destFile = absDestDir </> makeRelative srcDirInGitRoot fileInGitRoot
      createDirectoryIfMissing True (takeDirectory destFile)
      isSymlink <- pathIsSymbolicLink srcFile
      if isSymlink
        then getSymbolicLinkTarget srcFile >>= (`createFileLink` destFile)
        else Directory.copyFile srcFile destFile
