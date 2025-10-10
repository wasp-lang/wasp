{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Util.IO
  ( listDirectoryDeep,
    listDirectory,
    deleteDirectoryIfExists,
    deleteFileIfExists,
    doesFileExist,
    doesDirectoryExist,
    readFile,
    readFileStrict,
    writeFile,
    removeFile,
    copyFile,
    removeDirectory,
    copyDirectory,
    tryReadFile,
    isDirectoryEmpty,
    writeFileFromText,
    readFileBytes,
    writeFileBytes,
  )
where

import Control.Monad (filterM)
import Control.Monad.Extra (whenM)
import qualified Data.ByteString.Lazy as B
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text.IO as T.IO
import qualified Data.Text.IO as Text.IO
import qualified Path.IO as PathIO
import StrongPath (Abs, Dir, File, Path', Rel, basename, parseRelDir, parseRelFile, toFilePath, (</>))
import qualified StrongPath as SP
import qualified StrongPath.Path as SP.Path
import qualified System.Directory as SD
import qualified System.FilePath as FilePath
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)
import Prelude hiding (readFile, writeFile)
import qualified Prelude as P

-- TODO: write tests.

-- | Lists all files in the directory recursively.
-- All paths are relative to the directory we are listing.
-- If directory does not exist, returns empty list.
--
-- Example: Imagine we have directory foo that contains test.txt and bar/test2.txt.
-- If we call
-- >>> listDirectoryDeep "foo/"
-- we should get
-- >>> ["test.txt", "bar/text2.txt"]
listDirectoryDeep :: forall d f. Path' Abs (Dir d) -> IO [Path' (Rel d) (File f)]
listDirectoryDeep absDirPath = do
  (relFilePaths, relSubDirPaths) <-
    listDirectory absDirPath
      `catch` \e -> if isDoesNotExistError e then return ([], []) else throwIO e
  relSubDirFilesPaths <- mapM (listSubDirDeep . (absDirPath </>)) relSubDirPaths
  return $ relFilePaths ++ concat relSubDirFilesPaths
  where
    listSubDirDeep :: Path' Abs (Dir sd) -> IO [Path' (Rel d) (File f)]
    listSubDirDeep subDirPath = do
      files <- listDirectoryDeep subDirPath
      return $ map (basename subDirPath </>) files

-- TODO: write tests.

-- | Lists files and directories at top lvl of the directory.
listDirectory :: forall r d f. Path' Abs (Dir r) -> IO ([Path' (Rel r) (File f)], [Path' (Rel r) (Dir d)])
listDirectory absDirPath = do
  fpRelItemPaths <- SD.listDirectory fpAbsDirPath
  let sortedFpRelItemPaths = sort fpRelItemPaths
  relFilePaths <- filterFiles fpAbsDirPath sortedFpRelItemPaths
  relDirPaths <- filterDirs fpAbsDirPath sortedFpRelItemPaths
  return (relFilePaths, relDirPaths)
  where
    fpAbsDirPath :: FilePath
    fpAbsDirPath = toFilePath absDirPath

    filterFiles :: FilePath -> [FilePath] -> IO [Path' (Rel r) (File f)]
    filterFiles absDir relItems =
      filterM (SD.doesFileExist . (absDir FilePath.</>)) relItems
        >>= mapM parseRelFile

    filterDirs :: FilePath -> [FilePath] -> IO [Path' (Rel r) (Dir d)]
    filterDirs absDir relItems =
      filterM (SD.doesDirectoryExist . (absDir FilePath.</>)) relItems
        >>= mapM parseRelDir

-- The paths in the following functions intentionally aren't as polymorphic as
-- possible (i.e., they require 'Abs` paths). We prefer working with absolute
-- paths whenever possible (they make for a safe default). If you need to work
-- with relative paths, define a new function (e.g., `readFileRel`).

deleteDirectoryIfExists :: Path' Abs (Dir d) -> IO ()
deleteDirectoryIfExists dirPath =
  whenM (doesDirectoryExist dirPath) (removeDirectory dirPath)

deleteFileIfExists :: Path' Abs (File f) -> IO ()
deleteFileIfExists filePath = whenM (doesFileExist filePath) $ removeFile filePath

doesFileExist :: Path' Abs (File f) -> IO Bool
doesFileExist = SD.doesFileExist . SP.fromAbsFile

doesDirectoryExist :: Path' Abs (Dir f) -> IO Bool
doesDirectoryExist = SD.doesDirectoryExist . SP.fromAbsDir

readFile :: Path' Abs (File f) -> IO String
readFile = P.readFile . SP.fromAbsFile

readFileBytes :: Path' Abs (File f) -> IO B.ByteString
readFileBytes = B.readFile . SP.fromAbsFile

readFileStrict :: Path' Abs (File f) -> IO Text
readFileStrict = T.IO.readFile . SP.toFilePath

writeFile :: Path' Abs (File f) -> String -> IO ()
writeFile = P.writeFile . SP.fromAbsFile

writeFileBytes :: Path' Abs (File f) -> B.ByteString -> IO ()
writeFileBytes = B.writeFile . SP.fromAbsFile

writeFileFromText :: Path' Abs (File f) -> Text -> IO ()
writeFileFromText = T.IO.writeFile . SP.fromAbsFile

removeFile :: Path' Abs (File f) -> IO ()
removeFile = SD.removeFile . SP.fromAbsFile

copyFile :: Path' Abs (File f1) -> Path' Abs (File f2) -> IO ()
copyFile src dst = SD.copyFile (SP.fromAbsFile src) (SP.fromAbsFile dst)

removeDirectory :: Path' Abs (Dir d) -> IO ()
removeDirectory = SD.removeDirectoryRecursive . SP.fromAbsDir

copyDirectory :: Path' Abs (Dir d1) -> Path' Abs (Dir d2) -> IO ()
copyDirectory src dst = PathIO.copyDirRecur (SP.Path.toPathAbsDir src) (SP.Path.toPathAbsDir dst)

tryReadFile :: FilePath -> IO (Maybe Text)
tryReadFile fp =
  (Just <$> Text.IO.readFile fp)
    `catch` ( \e ->
                if isDoesNotExistError e
                  then return Nothing
                  else throwIO e
            )

isDirectoryEmpty :: Path' Abs (Dir d) -> IO Bool
isDirectoryEmpty dirPath = do
  (files, dirs) <- listDirectory dirPath
  return $ null files && null dirs
