module Util.IO
    ( listDirectoryDeep
    ) where

import qualified System.Directory as Dir
import System.FilePath ((</>), splitDirectories)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, throw)
import Control.Monad (filterM, mapM)


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
listDirectoryDeep :: FilePath -> IO [FilePath]
listDirectoryDeep dirPath = do
    dirItems <- Dir.listDirectory dirPath
                `catch` \e -> if isDoesNotExistError e then return [] else throw e
    files <- filterM (Dir.doesFileExist . (dirPath </>)) dirItems
    subDirs <- filterM (Dir.doesDirectoryExist . (dirPath </>)) dirItems
    subDirsFiles <- mapM (listSubDirDeep . (dirPath </>)) subDirs
    return $ files ++ (concat subDirsFiles)
  where
      getDirName :: FilePath -> FilePath
      getDirName path = last $ splitDirectories path

      -- | Returned paths are relative to the main dir whose sub dir we are listing.
      listSubDirDeep :: FilePath -> IO [FilePath]
      listSubDirDeep subDirPath = do
          paths <- listDirectoryDeep subDirPath
          return $ map ((getDirName subDirPath) </>) paths

      
