module Wasp.Generator.FileDraft.EmptyDirFileDraft
  ( EmptyDirFileDraft (..),
  )
where

import StrongPath (Dir', Path', Rel, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad (WriteableMonad (createDirectoryIfMissing))
import Wasp.Util (checksumFromText)

-- | File draft for a directory that has to exist, even when we generate
-- nothing into it.
newtype EmptyDirFileDraft = EmptyDirFileDraft
  { -- | Path where the dir will be created.
    _dstPath :: Path' (Rel GeneratedAppDir) Dir'
  }
  deriving (Show, Eq)

instance Writeable EmptyDirFileDraft where
  write dstDir draft =
    createDirectoryIfMissing True (SP.fromAbsDir $ dstDir </> _dstPath draft)

  getDstPath draft = Right $ _dstPath draft

  getChecksum _ = return $ checksumFromText ""
