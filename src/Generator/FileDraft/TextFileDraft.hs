module Generator.FileDraft.TextFileDraft
       ( TextFileDraft(..)
       ) where

import System.FilePath (FilePath, (</>), takeDirectory)

import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad

import Data.Text (Text)


-- | File draft based on text, that is to be written to file when time comes.
data TextFileDraft = TextFileDraft
    { -- | Path of file to be written, relative to some root dir.
      textFileDraftDstFilepath :: !FilePath
    , textFileDraftContent :: Text
    }
    deriving (Show, Eq)

instance Writeable TextFileDraft where
    write dstDir (TextFileDraft dstFilepath content) = do
        let dstAbsFilepath = dstDir </> dstFilepath
        createDirectoryIfMissing True (takeDirectory dstAbsFilepath)
        writeFileFromText dstAbsFilepath content
