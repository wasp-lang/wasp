module Generator.FileDraft.CopyFileDraft
       ( CopyFileDraft(..)
       ) where

import System.FilePath (FilePath, (</>), takeDirectory)

import Generator.FileDraft.WriteableToFile
import Generator.FileDraft.FileDraftIO

-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { -- | Path of file to be written, relative to some root dir.
      copyFileDraftDstFilepath :: !FilePath
      -- | Path of source file, relative to some root dir,
      --   normally not the same one as root dir for dstFilepath.
    , copyFileDraftSrcFilepath :: !FilePath
    }
    deriving (Show, Eq)

instance WriteableToFile CopyFileDraft where
    writeToFile dstDir (CopyFileDraft dstFilepath srcFilepath) = do
        let dstAbsFilepath = dstDir </> dstFilepath
        srcAbsFilepath <- getTemplateFileAbsPath srcFilepath
        createDirectoryIfMissing True (takeDirectory dstAbsFilepath)
        copyFile srcAbsFilepath dstAbsFilepath
