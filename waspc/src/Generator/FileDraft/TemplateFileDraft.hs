module Generator.FileDraft.TemplateFileDraft
       ( TemplateFileDraft(..)
       ) where

import qualified Data.Aeson as Aeson
import qualified Path
import qualified Path.Aliases as Path

import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad


-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { _dstPath :: !Path.RelFile -- ^ Path of file to be written, relative to some dst root dir.
    , _srcPathInTmplDir :: !Path.RelFile -- ^ Path of template source file, relative to templates root dir.
    , _tmplData :: Maybe Aeson.Value -- ^ Data to be fed to the template while rendering it.
    }
    deriving (Show, Eq)

instance Writeable TemplateFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (Path.toFilePath $ Path.parent absDraftDstPath)
        case _tmplData draft of
            Nothing -> do
                absDraftSrcPath <- getTemplateFileAbsPath (_srcPathInTmplDir draft)
                copyFile (Path.toFilePath absDraftSrcPath) (Path.toFilePath absDraftDstPath)
            Just tmplData -> do
                content <- compileAndRenderTemplate (_srcPathInTmplDir draft) tmplData
                writeFileFromText (Path.toFilePath absDraftDstPath) content
      where
        absDraftDstPath :: Path.AbsFile
        absDraftDstPath = absDstDirPath Path.</> (_dstPath draft)
