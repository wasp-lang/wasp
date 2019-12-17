module Generator.FileDraft.CopyDirDraft
       ( CopyDirDraft(..)
       ) where

import System.FilePath (FilePath, (</>))

import Generator.FileDraft.WriteableToFile
import Generator.FileDraft.FileDraftIO

-- | File draft based purely on another dir, that is just copied recursively with all the files in it.
data CopyDirDraft = CopyDirDraft
    { -- | Path where directory will be copied, relative to some root dir.
      dstPath :: !FilePath
      -- | Path of source directory.
    , srcPath :: !FilePath
    }
    deriving (Show, Eq)

instance WriteableToFile CopyDirDraft where
    writeToFile dstDir draft = do
        let dstAbsPath = dstDir </> (dstPath draft)
        copyDirectory (srcPath draft) dstAbsPath
