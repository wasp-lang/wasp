-- | File system steps. Relative paths are resolved against the working directory.
module E2E.Files
  ( writeToFile,
    appendToFile,
    replaceInFile,
    copyFile,
    deleteFile,
    deleteDirectory,
    copyGitTrackedDir,
  )
where

import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.Command (command)
import E2E.Process (ProcessResult (..), run)
import E2E.RunConfig (RunConfig (..))
import E2E.TestM (TestEnv (..), TestM, failTest, inDirectory, resolvePath)
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
  unless exists $ failTest $ "Expected " ++ path ++ " to exist, to append to it."
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
  liftIO (TE.decodeUtf8' <$> BS.readFile absPath) >>= \case
    Right content -> return content
    Left err -> failTest $ "Expected " ++ path ++ " to be UTF-8 text: " ++ show err

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
  srcDir <- asks ((</> srcDirInGitRoot) . (.runConfig.gitRootDir))
  absDstDir <- resolvePath dstDir
  -- Run in the source directory, so the paths are relative to it.
  trackedFiles <-
    map T.unpack . filter (not . T.null) . T.splitOn "\0" . (.stdout)
      <$> inDirectory srcDir (run $ command "git" ["ls-files", "-z"])
  liftIO $
    forM_ trackedFiles $ \file -> do
      createDirectoryIfMissing True (takeDirectory $ absDstDir </> file)
      Directory.copyFile (srcDir </> file) (absDstDir </> file)
