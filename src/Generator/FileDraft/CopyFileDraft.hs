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
      -- | Absolute path of source file.
    , copyFileDraftSrcFilepath :: !FilePath
    }
    deriving (Show, Eq)

instance Writeable CopyFileDraft where
    write dstDir (CopyFileDraft relDstPath absSrcPath) = do
        let absDstPath = dstDir </> relDstPath
        createDirectoryIfMissing True (takeDirectory absDstPath)
        copyFile absSrcPath absDstPath
