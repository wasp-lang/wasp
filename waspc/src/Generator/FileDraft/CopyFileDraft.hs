module Generator.FileDraft.CopyFileDraft
       ( CopyFileDraft(..)
       ) where

import qualified Path
import qualified Path.Aliases as Path

import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad


-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { _dstPath :: !Path.RelFile -- ^ Path of file to be written, relative to some root dir.
    , _srcPath :: !Path.AbsFile -- ^ Absolute path of source file to copy.
    }
    deriving (Show, Eq)

instance Writeable CopyFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (Path.toFilePath $ Path.parent absDraftDstPath)
        copyFile (Path.toFilePath $ _srcPath draft) (Path.toFilePath $ absDraftDstPath)
      where
          absDraftDstPath = absDstDirPath Path.</> (_dstPath draft)
