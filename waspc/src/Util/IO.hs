module Util.IO
  ( listDirectoryDeep,
    listDirectory,
  )
where

import Control.Monad (filterM)
import StrongPath (Abs, Dir, File, Path', Rel, basename, parseRelDir, parseRelFile, toFilePath, (</>))
import qualified System.Directory
import qualified System.FilePath as FilePath
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)

-- TODO: Convert these to use StrongPath?

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
listDirectoryDeep :: Path' Abs (Dir d) -> IO [Path' (Rel d) (File f)]
listDirectoryDeep absDirPath = do
  (relFilePaths, relSubDirPaths) <-
    listDirectory absDirPath
      `catch` \e -> if isDoesNotExistError e then return ([], []) else throwIO e
  relSubDirFilesPaths <- mapM (listSubDirDeep . (absDirPath </>)) relSubDirPaths
  return $ relFilePaths ++ concat relSubDirFilesPaths
  where
    listSubDirDeep :: Path' Abs (Dir d) -> IO [Path' (Rel r) (File f)]
    listSubDirDeep subDirPath = do
      files <- listDirectoryDeep subDirPath
      return $ map (basename subDirPath </>) files

-- TODO: write tests.

-- | Lists files and directories at top lvl of the directory.
listDirectory :: Path' Abs (Dir d) -> IO ([Path' (Rel r) (File f)], [Path' (Rel r) (Dir d)])
listDirectory absDirPath = do
  fpRelItemPaths <- System.Directory.listDirectory fpAbsDirPath
  relFilePaths <- filterFiles fpAbsDirPath fpRelItemPaths
  relDirPaths <- filterDirs fpAbsDirPath fpRelItemPaths
  return (relFilePaths, relDirPaths)
  where
    fpAbsDirPath :: FilePath
    fpAbsDirPath = toFilePath absDirPath

    filterFiles :: FilePath -> [FilePath] -> IO [Path' (Rel r) (File f)]
    filterFiles absDir relItems =
      filterM (System.Directory.doesFileExist . (absDir FilePath.</>)) relItems
        >>= mapM parseRelFile

    filterDirs :: FilePath -> [FilePath] -> IO [Path' (Rel r) (Dir d)]
    filterDirs absDir relItems =
      filterM (System.Directory.doesDirectoryExist . (absDir FilePath.</>)) relItems
        >>= mapM parseRelDir
