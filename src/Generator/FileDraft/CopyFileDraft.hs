module Generator.FileDraft.CopyFileDraft
       ( CopyFileDraft(..)
       ) where

import System.FilePath (FilePath, (</>), takeDirectory)

import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad

-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { -- | Path of file to be written, relative to some root dir.
      copyFileDraftDstFilepath :: !FilePath
      -- | Path of source file, relative to some root dir,
      --   normally not the same one as root dir for dstFilepath.
    , copyFileDraftSrcFilepath :: !FilePath
    }
    deriving (Show, Eq)

instance Writeable CopyFileDraft where
    write dstDir (CopyFileDraft dstFilepath srcFilepath) = do
        let dstAbsFilepath = dstDir </> dstFilepath
        srcAbsFilepath <- getTemplateFileAbsPath srcFilepath
        createDirectoryIfMissing True (takeDirectory dstAbsFilepath)
        copyFile srcAbsFilepath dstAbsFilepath
