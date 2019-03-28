module Generator.FileDraft.CopyFileDraft
       ( createCopyFileDraft
       ) where

import System.Directory (createDirectoryIfMissing, copyFile)
import System.FilePath (FilePath, (</>), takeDirectory)

import Generator.FileDraft
import qualified Generator.Templates as Templates


-- | File draft based purely on another file, that is just copied.
data CopyFileDraft = CopyFileDraft
    { -- | Path of file to be written, relative to some root dir.
      copyFileDraftDstFilepath :: !FilePath
      -- | Path of source file, relative to some root dir,
      --   normally not the same one as root dir for dstFilepath.
    , copyFileDraftSrcFilepath :: !FilePath
    }

instance WriteableToFile CopyFileDraft where
    writeToFile dstDir (CopyFileDraft dstFilepath srcFilepath) = do
        let absDstFilepath = dstDir </> dstFilepath
        absSrcFilepath <- Templates.getTemplateFileAbsPath srcFilepath
        createDirectoryIfMissing True (takeDirectory absDstFilepath)
        copyFile absSrcFilepath absDstFilepath

createCopyFileDraft :: FilePath -> FilePath -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraft $ CopyFileDraft dstPath srcPath
