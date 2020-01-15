module Generator.FileDraft.WriteableMonad
       ( WriteableMonad(..)
       ) where


import qualified System.Directory
import qualified Data.Text.IO
import Data.Aeson as Aeson
import Data.Text (Text)

import qualified Util.IO
import qualified Generator.Templates


-- TODO: Should we use DI via data instead of typeclasses?
--   https://news.ycombinator.com/item?id=10392044

-- | Describes effects needed by File Drafts.
class (Monad m) => WriteableMonad m where
    createDirectoryIfMissing
        :: Bool  -- ^ True if parents should also be created.
        -> FilePath  -- ^ Path to the directory to create.
        -> m ()

    copyFile
        :: FilePath  -- ^ Src path.
        -> FilePath  -- ^ Dst path.
        -> m ()

    writeFileFromText :: FilePath -> Text -> m ()

    -- | Copies all directory contents to the specified destination, recursively.
    --   Directory and sub directories are created as needed.
    --   Example: if we do `copyDirectory "/test" "/foo/bar"`, where /test contains files A.txt and B.txt,
    --     result will be creation of files A.txt and B.txt in /foo/bar directory. /foo and /foo/bar are
    --     also created if they did not exist before.
    copyDirectory
        :: FilePath  -- ^ Path of directory to be copied.
        -> FilePath -- ^ Path to a location where directory contents will be directly copied to.
        -> m ()

    getTemplateFileAbsPath
        :: FilePath  -- ^ Template file path, relative to templates root directory.
        -> m FilePath

    -- | Returns absolute path of templates root directory.
    getTemplatesDirAbsPath :: m FilePath

    compileAndRenderTemplate
        :: FilePath  -- ^ Path to the template file, relative to template root dir.
        -> Aeson.Value  -- ^ JSON to be provided as template data.
        -> m Text

instance WriteableMonad IO where
    createDirectoryIfMissing = System.Directory.createDirectoryIfMissing
    copyFile = System.Directory.copyFile
    writeFileFromText = Data.Text.IO.writeFile
    getTemplateFileAbsPath = Generator.Templates.getTemplateFileAbsPath
    getTemplatesDirAbsPath = Generator.Templates.getTemplatesDirAbsPath
    compileAndRenderTemplate = Generator.Templates.compileAndRenderTemplate
    copyDirectory = Util.IO.copyDirectory
