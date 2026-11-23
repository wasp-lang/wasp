-- | File system steps. Relative paths are resolved against the working directory.
module E2E.Files
  ( writeToFile,
    appendToFile,
    replaceInFile,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2E.TestM (TestM, failTest, resolvePath)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory)

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
