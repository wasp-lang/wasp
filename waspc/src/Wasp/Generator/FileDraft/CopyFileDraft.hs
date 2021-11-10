module Generator.FileDraft.CopyFileDraft
  ( CopyFileDraft (..),
  )
where

import Control.Monad (when)
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad
import StrongPath
  ( Abs,
    File',
    Path',
    Rel,
    (</>),
  )
import qualified StrongPath as SP
import System.IO.Error (doesNotExistErrorType, mkIOError)

-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
  { -- | Path where the file will be copied to.
    _dstPath :: !(Path' (Rel ProjectRootDir) File'),
    -- | Absolute path of source file to copy.
    _srcPath :: !(Path' Abs File'),
    _failIfSrcDoesNotExist :: Bool
  }
  deriving (Show, Eq)

instance Writeable CopyFileDraft where
  write absDstDirPath draft = do
    srcFileExists <- doesFileExist srcFilePath
    if srcFileExists
      then do
        createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent absDraftDstPath)
        copyFile srcFilePath (SP.fromAbsFile absDraftDstPath)
      else
        when
          (_failIfSrcDoesNotExist draft)
          ( throwIO $
              mkIOError
                doesNotExistErrorType
                "Source file of CopyFileDraft does not exist."
                Nothing
                (Just srcFilePath)
          )
    where
      srcFilePath = SP.fromAbsFile $ _srcPath draft
      absDraftDstPath = absDstDirPath </> _dstPath draft
