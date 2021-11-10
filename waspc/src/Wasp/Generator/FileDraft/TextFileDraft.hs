module Wasp.Generator.FileDraft.TextFileDraft
  ( TextFileDraft (..),
  )
where

import Data.Text (Text)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad
import StrongPath (File', Path', Rel, (</>))
import qualified StrongPath as SP

-- | File draft based on text, that is to be written to file when time comes.
data TextFileDraft = TextFileDraft
  { -- | Path where file will be generated.
    _dstPath :: !(Path' (Rel ProjectRootDir) File'),
    _content :: Text
  }
  deriving (Show, Eq)

instance Writeable TextFileDraft where
  write dstDir draft = do
    createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent absDraftDstPath)
    writeFileFromText (SP.fromAbsFile absDraftDstPath) (_content draft)
    where
      absDraftDstPath = dstDir </> _dstPath draft
