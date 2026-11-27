-- | File system steps. Relative paths are resolved against the working directory.
module E2E.Files
  ( writeToFile,
    appendToFile,
    replaceInFile,
    readJsonFile,
    copyFile,
    deleteFile,
    deleteDirectory,
    copyGitTrackedDir,
    waitForFile,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Command (program)
import E2E.Process (ProcessResult (..), RunningProcess, failIfExited, failWithOutput, run)
import E2E.SuiteEnv (TestSuiteEnv (..))
import E2E.TestM (TestEnv (..), TestM, failTest, inDirectory, resolvePath)
import E2E.Wait (pollUntil)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, removePathForcibly)
import qualified System.Directory as Directory
import System.FilePath (takeDirectory, (</>))

-- | Creates or overwrites the file, along with any missing parent directories.
writeToFile :: FilePath -> T.Text -> TestM ()
writeToFile path content = do
  absPath <- resolvePath path
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory absPath)
    BS.writeFile absPath (TE.encodeUtf8 content)

-- | Appends the content and a newline to the file. Fails if the file doesn't
-- exist, since that usually means the test edits the wrong file. Use
-- 'writeToFile' to create one.
appendToFile :: FilePath -> T.Text -> TestM ()
appendToFile path content = do
  absPath <- resolvePath path
  exists <- liftIO $ doesFileExist absPath
  unless exists $ failTest $ "Expected " ++ absPath ++ " to exist, to append to it."
  liftIO $ BS.appendFile absPath (TE.encodeUtf8 $ content <> "\n")

-- | Replaces all occurrences of the text. Fails if the text isn't in the file.
replaceInFile :: FilePath -> T.Text -> T.Text -> TestM ()
replaceInFile path oldText newText = do
  content <- readFromFile path
  absPath <- resolvePath path
  unless (oldText `T.isInfixOf` content)
    $ failTest
    $ "Expected " ++ absPath ++ " to contain " ++ show oldText ++ "."
  writeToFile path (T.replace oldText newText content)

readFromFile :: FilePath -> TestM T.Text
readFromFile path = do
  absPath <- resolvePath path
  liftIO (TE.decodeUtf8' <$> BS.readFile absPath) >>= \case
    Right content -> return content
    Left err -> failTest $ "Expected " ++ absPath ++ " to be UTF-8 text: " ++ show err

-- | The file decoded as JSON, into a data type with the fields to check.
-- Fails the test if it doesn't decode.
readJsonFile :: (Aeson.FromJSON a) => FilePath -> TestM a
readJsonFile path = do
  absPath <- resolvePath path
  liftIO (Aeson.eitherDecodeFileStrict absPath)
    >>= either (failTest . (("Expected " ++ absPath ++ " to be JSON that decodes: ") ++)) return

copyFile :: FilePath -> FilePath -> TestM ()
copyFile srcPath dstPath = do
  absSrcPath <- resolvePath srcPath
  absDstPath <- resolvePath dstPath
  liftIO $ Directory.copyFile absSrcPath absDstPath

deleteFile :: FilePath -> TestM ()
deleteFile path = resolvePath path >>= liftIO . removeFile

-- | Deletes the directory with all of its contents, if it exists.
deleteDirectory :: FilePath -> TestM ()
deleteDirectory path = resolvePath path >>= liftIO . removePathForcibly

-- | Copies the git-tracked files of a directory in the Wasp repo into the
-- destination directory. Ignored and untracked files (e.g. @node_modules@,
-- build outputs) are left out. Symlinks are copied as the files they point to.
copyGitTrackedDir :: FilePath -> FilePath -> TestM ()
copyGitTrackedDir srcDirInGitRoot dstDir = do
  srcDir <- asks ((</> srcDirInGitRoot) . (.testSuiteEnv.gitRootDir))
  absDstDir <- resolvePath dstDir
  -- Run in the source directory, so the paths are relative to it.
  trackedFiles <-
    map T.unpack . filter (not . T.null) . T.splitOn "\0" . (.stdout)
      <$> inDirectory srcDir (run $ program "git" ["ls-files", "-z"])
  liftIO $
    forM_ trackedFiles $ \file -> do
      createDirectoryIfMissing True (takeDirectory $ absDstDir </> file)
      Directory.copyFile (srcDir </> file) (absDstDir </> file)

-- | Waits until the process creates the file. Fails the test with the
-- process's output if it exits first or doesn't create the file in time.
waitForFile :: RunningProcess -> FilePath -> Int -> TestM ()
waitForFile runningProcess path seconds = do
  absPath <- resolvePath path
  exists <- pollUntil seconds $ do
    exists <- liftIO $ doesFileExist absPath
    unless exists $ failIfExited runningProcess ("creating " ++ path)
    return exists
  unless exists $
    failWithOutput runningProcess ("didn't create " ++ path ++ " within " ++ show seconds ++ "s")
