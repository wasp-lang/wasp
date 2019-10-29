module Generator.FileDraft.FileDraftIO
       ( FileDraftIO(..)
       ) where


import qualified System.Directory
import qualified Data.Text.IO
import Data.Aeson as Aeson
import Data.Text (Text)

import qualified Generator.Templates


-- TODO: Should we use DI via data instead of typeclasses?
--   https://news.ycombinator.com/item?id=10392044

-- | Describes effects needed by File Drafts.
class (Monad m) => FileDraftIO m where
    createDirectoryIfMissing
        :: Bool  -- ^ True if parents should also be created.
        -> FilePath  -- ^ Path to the directory to create.
        -> m ()

    copyFile
        :: FilePath  -- ^ Src path.
        -> FilePath  -- ^ Dst path.
        -> m ()

    writeFileFromText :: FilePath -> Text -> m ()

    getTemplateFileAbsPath
        :: FilePath  -- ^ Template file path, relative to templates root directory.
        -> m FilePath

    -- | Returns absolute path of templates root directory.
    getTemplatesDirAbsPath :: m FilePath

    compileAndRenderTemplate
        :: FilePath  -- ^ Path to the template file, relative to template root dir.
        -> Aeson.Value  -- ^ JSON to be provided as template data.
        -> m Text

instance FileDraftIO IO where
    createDirectoryIfMissing = System.Directory.createDirectoryIfMissing
    copyFile = System.Directory.copyFile
    writeFileFromText = Data.Text.IO.writeFile
    getTemplateFileAbsPath = Generator.Templates.getTemplateFileAbsPath
    getTemplatesDirAbsPath = Generator.Templates.getTemplatesDirAbsPath
    compileAndRenderTemplate = Generator.Templates.compileAndRenderTemplate
