module Generator.FileDraft.TemplateFileDraft
  ( TemplateFileDraft (..),
  )
where

import qualified Data.Aeson as Aeson
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.Writeable
import Generator.FileDraft.WriteableMonad
import Generator.Templates (TemplatesDir)
import StrongPath (Abs, File', Path', Rel, (</>))
import qualified StrongPath as SP

-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
  { -- | Path where file will be generated.
    _dstPath :: !(Path' (Rel ProjectRootDir) File'),
    -- | Path of template source file.
    _srcPathInTmplDir :: !(Path' (Rel TemplatesDir) File'),
    -- | Data to be fed to the template while rendering it.
    _tmplData :: Maybe Aeson.Value
  }
  deriving (Show, Eq)

instance Writeable TemplateFileDraft where
  write absDstDirPath draft = do
    createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent absDraftDstPath)
    case _tmplData draft of
      Nothing -> do
        absDraftSrcPath <- getTemplateFileAbsPath (_srcPathInTmplDir draft)
        copyFile (SP.fromAbsFile absDraftSrcPath) (SP.fromAbsFile absDraftDstPath)
      Just tmplData -> do
        content <- compileAndRenderTemplate (_srcPathInTmplDir draft) tmplData
        writeFileFromText (SP.toFilePath absDraftDstPath) content
    where
      absDraftDstPath :: Path' Abs File'
      absDraftDstPath = absDstDirPath </> _dstPath draft
