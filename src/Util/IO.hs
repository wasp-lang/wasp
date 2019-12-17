module Util.IO
    ( copyDirectory
    ) where

import System.Directory (listDirectory, doesDirectoryExist, copyFile, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Control.Exception (catch, throw)
import Control.Monad (sequence)

-- | Copies all directory contents to specified destination, recursively.
--   Directory and sub directories are created as needed.
--   If directory does not exist, does nothing (does not fail).
copyDirectory
    :: FilePath  -- ^ Path to directory to be copied.
    -> FilePath  -- ^ Path to location where directory contents will be copied to.
    -> IO ()
copyDirectory dirSrcPath dirDstPath = do
    names <- listDirectory dirSrcPath
        `catch` \e -> if isDoesNotExistError e then return [] else throw e
    if length names == 0 then return () else do
        isDirFlags <- sequence $ map doesDirectoryExist names
        let dirNames = map fst $ filter snd $ zip names isDirFlags
        let fileNames = map fst $ filter (not . snd) $ zip names isDirFlags
        createDirectoryIfMissing True dirDstPath
        sequence_ $ map (\name -> copyFile (dirSrcPath </> name) (dirDstPath </> name)) fileNames
        sequence_ $ map (\name -> copyDirectory (dirSrcPath </> name) (dirDstPath </> name)) dirNames
        return ()
