module Generator.FileDraft.TextFileDraft
       ( TextFileDraft(..)
       ) where

import Data.Text (Text)

import StrongPath (Path, Rel, File, (</>))
import qualified StrongPath as SP
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad


-- | File draft based on text, that is to be written to file when time comes.
data TextFileDraft = TextFileDraft
    { _dstPath :: !(Path (Rel ProjectRootDir) File) -- ^ Path where file will be generated.
    , _content :: Text
    }
    deriving (Show, Eq)

instance Writeable TextFileDraft where
    write dstDir draft = do
        createDirectoryIfMissing True (SP.toFilePath $ SP.parent absDraftDstPath)
        writeFileFromText (SP.toFilePath absDraftDstPath) (_content draft)
      where
          absDraftDstPath = dstDir </> (_dstPath draft)
