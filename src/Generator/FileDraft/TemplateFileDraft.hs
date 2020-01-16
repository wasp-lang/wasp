module Generator.FileDraft.TemplateFileDraft
       ( TemplateFileDraft(..)
       ) where

import System.FilePath (FilePath, (</>), takeDirectory)
import qualified Data.Aeson as Aeson

import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad


-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { -- | Path of file to be written, relative to some dst root dir.
      templateFileDraftDstFilepath :: !FilePath
      -- | Path of template source file, relative to templates root dir.
    , templateFileDraftTemplateRelFilepath :: !FilePath
      -- | Data to be fed to the template while rendering it.
    , templateFileDraftTemplateData :: Maybe Aeson.Value
    }
    deriving (Show, Eq)

instance Writeable TemplateFileDraft where
    write dstDir draft = do
        createDirectoryIfMissing True (takeDirectory absDstPath)
        case templateFileDraftTemplateData draft of
            Nothing -> do
                absSrcPath <- getTemplateFileAbsPath templateSrcPathInTemplateDir
                copyFile absSrcPath absDstPath
            Just tmplData -> do
                content <- compileAndRenderTemplate templateSrcPathInTemplateDir tmplData
                writeFileFromText absDstPath content
      where
        templateSrcPathInTemplateDir :: FilePath
        templateSrcPathInTemplateDir = templateFileDraftTemplateRelFilepath draft

        absDstPath :: FilePath
        absDstPath = dstDir </> (templateFileDraftDstFilepath draft)
