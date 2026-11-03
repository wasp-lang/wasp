-- | File system steps. Relative paths are resolved against the working directory.
module E2E.Files
  ( writeToFile,
    appendToFile,
    replaceInFile,
    readFromFile,
    deleteFile,
    deleteDirectory,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.TestM (TestM, failTest, resolvePath)
import System.Directory (createDirectoryIfMissing, removeFile, removePathForcibly)
import System.FilePath (takeDirectory)

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

deleteFile :: FilePath -> TestM ()
deleteFile path = resolvePath path >>= liftIO . removeFile

-- | Deletes the directory with all of its contents, if it exists.
deleteDirectory :: FilePath -> TestM ()
deleteDirectory path = resolvePath path >>= liftIO . removePathForcibly
