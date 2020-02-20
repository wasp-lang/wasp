module Generator.FileDraft.TextFileDraft
       ( TextFileDraft(..)
       ) where

import qualified Path
import qualified Path.Aliases as Path

import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad

import Data.Text (Text)


-- | File draft based on text, that is to be written to file when time comes.
data TextFileDraft = TextFileDraft
    { _dstPath :: !Path.RelFile -- ^ Path of file to be written, relative to some root dir.
    , _content :: Text
    }
    deriving (Show, Eq)

instance Writeable TextFileDraft where
    write dstDir draft = do
        createDirectoryIfMissing True (Path.toFilePath $ Path.parent absDraftDstPath)
        writeFileFromText (Path.toFilePath absDraftDstPath) (_content draft)
      where
          absDraftDstPath = dstDir Path.</> (_dstPath draft)
