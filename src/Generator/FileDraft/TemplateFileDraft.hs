module Generator.FileDraft.TemplateFileDraft
       ( TemplateFileDraft(..)
       ) where

import System.FilePath (FilePath, (</>), takeDirectory)
import Data.Text (Text)
import qualified Data.Aeson as Aeson

import Generator.FileDraft.WriteableToFile
import Generator.FileDraft.FileDraftIO


-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { -- | Path of file to be written, relative to some dst root dir.
      templateFileDraftDstFilepath :: !FilePath
      -- | Path of template source file, relative to templates root dir.
    , templateFileDraftTemplateRelFilepath :: !FilePath
      -- | Data to be fed to the template while rendering it.
    , templateFileDraftTemplateData :: !Aeson.Value
    }
    deriving (Show, Eq)

instance WriteableToFile TemplateFileDraft where
    writeToFile dstDir draft =
        compileAndRenderTemplate templateRelFilepath templateData >>= writeContentToFile
      where
        templateRelFilepath :: FilePath
        templateRelFilepath = templateFileDraftTemplateRelFilepath draft

        templateData :: Aeson.Value
        templateData = templateFileDraftTemplateData draft

        writeContentToFile :: (FileDraftIO m) => Text -> m ()
        writeContentToFile content = do
            let absDstFilepath = dstDir </> (templateFileDraftDstFilepath draft)
            createDirectoryIfMissing True (takeDirectory absDstFilepath)
            writeFileFromText absDstFilepath content
