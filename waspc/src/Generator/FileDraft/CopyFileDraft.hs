module Generator.FileDraft.CopyFileDraft
       ( CopyFileDraft(..)
       ) where

import StrongPath (Path, Abs, Rel, File, (</>))
import qualified StrongPath as SP
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad


-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { _dstPath :: !(Path (Rel ProjectRootDir) File)-- ^ Path where the file will be copied to.
    , _srcPath :: !(Path Abs File) -- ^ Absolute path of source file to copy.
    }
    deriving (Show, Eq)

instance Writeable CopyFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (SP.toFilePath $ SP.parent absDraftDstPath)
        copyFile (SP.toFilePath $ _srcPath draft) (SP.toFilePath $ absDraftDstPath)
      where
          absDraftDstPath = absDstDirPath </> (_dstPath draft)
