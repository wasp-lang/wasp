module Wasp.Generator.FileDraft.CopyDirFileDraft
  ( CopyDirFileDraft (..),
  )
where

import Control.Monad (when)
import StrongPath
  ( Abs,
    Dir',
    Path',
    Rel,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad (WriteableMonad (copyDirectoryRecursive, createDirectoryIfMissing), doesDirectoryExist)

-- | File draft based on another dir that is to be recursively copied.
--
-- TODO: revisit the quick and dirty Linux interpretation of "everything is a file"
-- and treating a directory (`CopyDirFileDraft`) as a `FileDraft`. As is, this may be
-- a source of potential confusion and possibly tech debt to resolve later.
data CopyDirFileDraft = CopyDirFileDraft
  { -- | Path where the dir will be copied to.
    _dstPath :: !(Path' (Rel ProjectRootDir) Dir'),
    -- | Absolute path of source dir to copy.
    _srcPath :: !(Path' Abs Dir')
  }
  deriving (Show, Eq)

instance Writeable CopyDirFileDraft where
  write projectRootAbsPath draft = do
    srcDirExists <- doesDirectoryExist $ SP.fromAbsDir srcPathAbsDir
    when srcDirExists $ do
      createDirectoryIfMissing True (SP.fromAbsDir dstPathAbsDir)
      copyDirectoryRecursive srcPathAbsDir dstPathAbsDir
    where
      srcPathAbsDir = _srcPath draft
      dstPathAbsDir = projectRootAbsPath </> _dstPath draft
