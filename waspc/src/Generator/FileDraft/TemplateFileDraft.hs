module Generator.FileDraft.TemplateFileDraft
       ( TemplateFileDraft(..)
       ) where

import qualified Data.Aeson as Aeson

import StrongPath (Path, Abs, Rel, File, (</>))
import qualified StrongPath as SP
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad
import Generator.Templates (TemplatesDir)

-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { _dstPath :: !(Path (Rel ProjectRootDir) File) -- ^ Path where file will be generated.
    , _srcPathInTmplDir :: !(Path (Rel TemplatesDir) File) -- ^ Path of template source file.
    , _tmplData :: Maybe Aeson.Value -- ^ Data to be fed to the template while rendering it.
    }
    deriving (Show, Eq)

instance Writeable TemplateFileDraft where
    write absDstDirPath draft = do
        createDirectoryIfMissing True (SP.toFilePath $ SP.parent absDraftDstPath)
        case _tmplData draft of
            Nothing -> do
                absDraftSrcPath <- getTemplateFileAbsPath (_srcPathInTmplDir draft)
                copyFile (SP.toFilePath absDraftSrcPath) (SP.toFilePath absDraftDstPath)
            Just tmplData -> do
                content <- compileAndRenderTemplate (_srcPathInTmplDir draft) tmplData
                writeFileFromText (SP.toFilePath absDraftDstPath) content
      where
        absDraftDstPath :: Path Abs File
        absDraftDstPath = absDstDirPath </> (_dstPath draft)
