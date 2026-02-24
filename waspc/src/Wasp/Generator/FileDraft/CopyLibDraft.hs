module Wasp.Generator.FileDraft.CopyLibDraft
  ( CopyLibDraft (..),
  )
where

import qualified Data.ByteString as BS
import StrongPath (Abs, File', Path', Rel, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad
import Wasp.Generator.WaspLibs.Common (LibsSourceDir, getAbsLibsSourceDirPath)
import Wasp.Util (checksumFromByteString)

-- | File draft based on a library file that is copied to the generated project.
data CopyLibDraft = CopyLibDraft
  { -- | Path where the file will be copied to.
    _dstPath :: !(Path' (Rel ProjectRootDir) File'),
    -- | Path of source file relative to the libs source directory.
    _srcPathInLibsSourceDir :: !(Path' (Rel LibsSourceDir) File')
  }
  deriving (Show, Eq)

instance Writeable CopyLibDraft where
  write absDstDirPath draft = do
    libsSourceDirAbsPath <- getLibsSourceDirAbsPath
    let absSrcPath = libsSourceDirAbsPath </> _srcPathInLibsSourceDir draft
    createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent absDraftDstPath)
    copyFile (SP.fromAbsFile absSrcPath) (SP.fromAbsFile absDraftDstPath)
    where
      absDraftDstPath :: Path' Abs File'
      absDraftDstPath = absDstDirPath </> _dstPath draft

  getChecksum draft = do
    libsSourceDirAbsPath <- getAbsLibsSourceDirPath
    let absSrcPath = libsSourceDirAbsPath </> _srcPathInLibsSourceDir draft
    checksumFromByteString <$> BS.readFile (SP.fromAbsFile absSrcPath)

  getDstPath draft = Left $ _dstPath draft
