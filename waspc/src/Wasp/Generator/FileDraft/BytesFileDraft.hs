module Wasp.Generator.FileDraft.BytesFileDraft
  ( BytesFileDraft (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import StrongPath (File', Path', Rel, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad

-- | File draft based on raw bytes
data BytesFileDraft = BytesFileDraft
  { -- | Path where file will be generated.
    _dstPath :: !(Path' (Rel ProjectRootDir) File'),
    _content :: ByteString
  }
  deriving (Show, Eq)

instance Writeable BytesFileDraft where
  write dstDir draft = do
    createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent absDraftDstPath)
    writeFileFromByteString (SP.fromAbsFile absDraftDstPath) content
    where
      absDraftDstPath = dstDir </> _dstPath draft
      content = _content draft
