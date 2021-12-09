{-# LANGUAGE TupleSections #-}

module Wasp.Generator.FileDraft.TemplateFileDraft
  ( TemplateFileDraft (..),
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.Text
import StrongPath (Abs, File', Path', Rel, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.FileDraft.WriteableMonad
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Util (checksumFromByteString, checksumFromText)

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
    processTmpl
      draft
      (\absDraftSrcPath -> copyFile (SP.fromAbsFile absDraftSrcPath) (SP.fromAbsFile absDraftDstPath))
      (writeFileFromText $ SP.toFilePath absDraftDstPath)
    where
      absDraftDstPath :: Path' Abs File'
      absDraftDstPath = absDstDirPath </> _dstPath draft

  -- TODO: we are doing reading of the file twice. Once here, once above in write.
  getChecksum draft = do
    processTmpl
      draft
      ((checksumFromByteString <$>) . BS.readFile . SP.fromAbsFile)
      (return . checksumFromText)

  getDstPath draft = Left $ _dstPath draft

processTmpl :: WriteableMonad m => TemplateFileDraft -> (Path' Abs File' -> m b) -> (Text -> m b) -> m b
processTmpl draft onNoTmplData onTmplData = case _tmplData draft of
  Nothing -> do
    absDraftSrcPath <- getTemplateFileAbsPath (_srcPathInTmplDir draft)
    onNoTmplData absDraftSrcPath
  Just tmplData -> do
    content <- compileAndRenderTemplate (_srcPathInTmplDir draft) tmplData
    onTmplData content
