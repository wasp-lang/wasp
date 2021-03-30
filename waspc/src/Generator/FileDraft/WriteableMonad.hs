module Generator.FileDraft.WriteableMonad
       ( WriteableMonad(..)
       ) where


import UnliftIO.Exception (catch, throwIO)
import System.IO.Error (isDoesNotExistError)
import qualified System.Directory
import qualified Data.Text.IO
import Data.Aeson as Aeson
import Data.Text (Text)

import StrongPath (Path, Abs, Rel, File, Dir)
import qualified Generator.Templates as Templates


-- TODO: Should we use DI via data instead of typeclasses?
--   https://news.ycombinator.com/item?id=10392044

-- TODO: Should we make constraint MonadIO instead of just Monad?
--   That would allow us to do liftIO. And that might allow us to perform any IO
--   we want (hm will it?), which could be useful for custom stuff (but does that defeat the whole purpose?).
--   But that means we can't test that part, which yes, defeats the purpose somewhat.
--   I feel like all together we should not do it :), but it is an option if needed.

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

    getTemplateFileAbsPath
        :: Path (Rel Templates.TemplatesDir) File  -- ^ Template file path.
        -> m (Path Abs File)

    getTemplatesDirAbsPath :: m (Path Abs (Dir Templates.TemplatesDir))

    compileAndRenderTemplate
        :: Path (Rel Templates.TemplatesDir) File  -- ^ Template file path.
        -> Aeson.Value  -- ^ JSON to be provided as template data.
        -> m Text

instance WriteableMonad IO where
    createDirectoryIfMissing = System.Directory.createDirectoryIfMissing
    -- TODO(matija): we should rename this function to make it clear it won't throw an exception when
    -- a file does not exist.
    copyFile src dst = do
        -- NOTE(matija): we had cases (e.g. tmp Vim files) where a file initially existed
        -- when the filedraft was created but then got deleted before actual copying was invoked.
        -- That would make this function crash, so we just ignore those errors.
        System.Directory.copyFile src dst `catch` (\e -> if isDoesNotExistError e
                                                         then return ()
                                                         else throwIO e)

    writeFileFromText = Data.Text.IO.writeFile
    getTemplateFileAbsPath = Templates.getTemplateFileAbsPath
    getTemplatesDirAbsPath = Templates.getTemplatesDirAbsPath
    compileAndRenderTemplate = Templates.compileAndRenderTemplate
