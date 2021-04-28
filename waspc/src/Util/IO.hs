module Util.IO
  ( listDirectoryDeep,
    listDirectory,
  )
where

import Control.Monad (filterM)
import qualified Path as P
import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)

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
listDirectoryDeep :: P.Path P.Abs P.Dir -> IO [P.Path P.Rel P.File]
listDirectoryDeep absDirPath = do
  (relFilePaths, relSubDirPaths) <-
    listDirectory absDirPath
      `catch` \e -> if isDoesNotExistError e then return ([], []) else throwIO e
  relSubDirFilesPaths <- mapM (listSubDirDeep . (absDirPath P.</>)) relSubDirPaths
  return $ relFilePaths ++ concat relSubDirFilesPaths
  where
    listSubDirDeep :: P.Path P.Abs P.Dir -> IO [P.Path P.Rel P.File]
    listSubDirDeep subDirPath = do
      files <- listDirectoryDeep subDirPath
      return $ map (P.dirname subDirPath P.</>) files

-- TODO: write tests.

-- | Lists files and directories at top lvl of the directory.
listDirectory :: P.Path P.Abs P.Dir -> IO ([P.Path P.Rel P.File], [P.Path P.Rel P.Dir])
listDirectory absDirPath = do
  fpRelItemPaths <- Dir.listDirectory fpAbsDirPath
  relFilePaths <- filterFiles fpAbsDirPath fpRelItemPaths
  relDirPaths <- filterDirs fpAbsDirPath fpRelItemPaths
  return (relFilePaths, relDirPaths)
  where
    fpAbsDirPath :: FilePath
    fpAbsDirPath = P.toFilePath absDirPath

    filterFiles :: FilePath -> [FilePath] -> IO [P.Path P.Rel P.File]
    filterFiles absDir relItems =
      filterM (Dir.doesFileExist . (absDir FilePath.</>)) relItems
        >>= mapM P.parseRelFile

    filterDirs :: FilePath -> [FilePath] -> IO [P.Path P.Rel P.Dir]
    filterDirs absDir relItems =
      filterM (Dir.doesDirectoryExist . (absDir FilePath.</>)) relItems
        >>= mapM P.parseRelDir
