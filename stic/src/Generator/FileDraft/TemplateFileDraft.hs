module Generator.FileDraft.TemplateFileDraft
       ( createTemplateFileDraft
       ) where

import System.Directory (createDirectoryIfMissing, copyFile)
import System.FilePath (FilePath, (</>), takeDirectory)
import Control.Monad (when)
import Data.Text (Text)
import qualified Text.Mustache as Mustache
import qualified Data.Aeson as Aeson
import qualified Data.Text.IO as TextIO

import Generator.FileDraft
import qualified Generator.Templates as Templates


-- | File draft based on template file that gets combined with data.
data TemplateFileDraft = TemplateFileDraft
    { -- | Path of file to be written, relative to some root dir.
      tmplFileDraftDstFilepath :: !FilePath
      -- | Path of template source file, relative to some root dir,
      --   normally not the same one as root dir for dstFilepath.
    , tmplFileDraftTemplateFilepath :: !FilePath
      -- | Data to be fed to the template while rendering it.
    , tmplFileDraftData :: !Aeson.Value
    }

instance WriteableToFile TemplateFileDraft where
    writeToFile dstDir draft = renderTemplate >>= writeDraftToFile
      where
        renderTemplate :: IO Text
        renderTemplate = do
            mustacheTemplate <- compileMustacheTemplate (tmplFileDraftTemplateFilepath draft)
            let mustacheTemplateData = Mustache.toMustache (tmplFileDraftData draft)
            let (errors, fileText) = Mustache.checkedSubstituteValue mustacheTemplate mustacheTemplateData
            -- TODO: Handle these errors better.
            if (null errors)
                then (return fileText)
                else (error "Error occured while rendering template")

        writeDraftToFile :: Text -> IO ()
        writeDraftToFile content = do
            let absDstFilepath = dstDir </> (tmplFileDraftDstFilepath draft)
            createDirectoryIfMissing True (takeDirectory absDstFilepath)
            TextIO.writeFile absDstFilepath content

createTemplateFileDraft :: FilePath -> FilePath -> Aeson.Value -> FileDraft
createTemplateFileDraft dstPath tmplRelPath tmplData =
    FileDraft $ TemplateFileDraft dstPath tmplRelPath tmplData

compileMustacheTemplate
    :: FilePath  -- ^ Path of template file, relative to root dir of templates.
    -> IO Mustache.Template
compileMustacheTemplate path = do
    templatesDirAbsPath <- Templates.getTemplatesDirAbsPath
    templateAbsPath <- Templates.getTemplateFileAbsPath path
    eitherTemplate <- Mustache.automaticCompile [templatesDirAbsPath] templateAbsPath
    return $ either
        (\e -> error $ "Compilation of template (" ++ path ++ ") failed." ++ (show e))
        id
        eitherTemplate
