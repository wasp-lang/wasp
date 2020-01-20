module Util.IO
    ( listDirectoryDeep
    ) where

import qualified System.Directory as Dir
import qualified System.FilePath as FilePath
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, throw)
import Control.Monad (filterM, mapM)
import qualified Path

import qualified Path.Aliases as Path


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
listDirectoryDeep :: Path.AbsDir -> IO [Path.RelFile]
listDirectoryDeep absDirPath = do
    (relFilePaths, relSubDirPaths) <- (listDirectory absDirPath)
        `catch` \e -> if isDoesNotExistError e then return ([], []) else throw e
    relSubDirFilesPaths <- mapM (listSubDirDeep . (absDirPath Path.</>)) relSubDirPaths
    return $ relFilePaths ++ (concat relSubDirFilesPaths)
  where
      -- | NOTE: Here, returned paths are relative to the main dir whose sub dir we are listing,
      --   which is one level above what you might intuitively expect.
      listSubDirDeep :: Path.AbsDir -> IO [Path.RelFile]
      listSubDirDeep subDirPath = do
          files <- listDirectoryDeep subDirPath
          return $ map ((Path.dirname subDirPath) Path.</>) files

-- TODO: write tests.
-- | Lists files and directories at top lvl of the directory.
listDirectory :: Path.AbsDir -> IO ([Path.RelFile], [Path.RelDir])
listDirectory absDirPath = do
    fpRelItemPaths <- Dir.listDirectory fpAbsDirPath
    relFilePaths <- filterFiles fpAbsDirPath fpRelItemPaths
    relDirPaths <- filterDirs fpAbsDirPath fpRelItemPaths
    return (relFilePaths, relDirPaths)
  where
      fpAbsDirPath :: FilePath
      fpAbsDirPath = Path.toFilePath absDirPath

      filterFiles :: FilePath -> [FilePath] -> IO [Path.RelFile]
      filterFiles absDir relItems = filterM (Dir.doesFileExist . (absDir FilePath.</>)) relItems
                                    >>= mapM Path.parseRelFile

      filterDirs :: FilePath -> [FilePath] -> IO [Path.RelDir]
      filterDirs absDir relItems = filterM (Dir.doesDirectoryExist . (absDir FilePath.</>)) relItems
                                   >>= mapM Path.parseRelDir
