module Wasp.Generator.FileDraft.WriteableMonad
  ( WriteableMonad (..),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text.IO
import qualified Path.IO as PathIO
import StrongPath (Abs, Dir, File, Path', Rel)
import qualified StrongPath.Path as SP.Path
import qualified System.Directory
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (Exception, catch)
import qualified UnliftIO.Exception as E
import qualified Wasp.Generator.Templates as Templates

-- TODO: Should we use DI via data instead of typeclasses?
--   https://news.ycombinator.com/item?id=10392044

-- | Describes effects needed by File Drafts.
class (MonadIO m) => WriteableMonad m where
  createDirectoryIfMissing ::
    -- | True if parents should also be created.
    Bool ->
    -- | Path to the directory to create.
    FilePath ->
    m ()

  copyFile ::
    -- | Src path.
    FilePath ->
    -- | Dst path.
    FilePath ->
    m ()

  -- | Copies a directory recursively.
  -- It does not follow symbolic links and preserves permissions when possible.
  copyDirectoryRecursive ::
    -- | Src path.
    Path' Abs (Dir a) ->
    -- | Dst path.
    Path' Abs (Dir b) ->
    m ()

  -- |  Removes an existing directory dir together with its contents and sub-directories.
  -- Within this directory, symbolic links are removed without affecting their targets.
  removeDirRecur :: Path' Abs (Dir b) -> m ()

  doesFileExist :: FilePath -> m Bool

  doesDirectoryExist :: FilePath -> m Bool

  writeFileFromText :: FilePath -> Text -> m ()

  getTemplateFileAbsPath ::
    -- | Template file path.
    Path' (Rel Templates.TemplatesDir) (File a) ->
    m (Path' Abs (File a))

  getTemplatesDirAbsPath :: m (Path' Abs (Dir Templates.TemplatesDir))

  compileAndRenderTemplate ::
    -- | Template file path.
    Path' (Rel Templates.TemplatesDir) (File a) ->
    -- | JSON to be provided as template data.
    Aeson.Value ->
    m Text

  throwIO :: (Exception e) => e -> m a

instance WriteableMonad IO where
  createDirectoryIfMissing = System.Directory.createDirectoryIfMissing

  -- TODO(matija): we should rename this function to make it clear it won't throw an exception when
  -- a file does not exist.
  copyFile src dst = do
    -- NOTE(matija): we had cases (e.g. tmp Vim files) where a file initially existed
    -- when the filedraft was created but then got deleted before actual copying was invoked.
    -- That would make this function crash, so we just ignore those errors.
    System.Directory.copyFile src dst
      `catch` ( \e ->
                  if isDoesNotExistError e
                    then return ()
                    else throwIO e
              )

  copyDirectoryRecursive src dst = do
    PathIO.copyDirRecur (SP.Path.toPathAbsDir src) (SP.Path.toPathAbsDir dst)

  removeDirRecur dir = PathIO.removeDirRecur (SP.Path.toPathAbsDir dir)

  doesFileExist = System.Directory.doesFileExist
  doesDirectoryExist = System.Directory.doesDirectoryExist
  writeFileFromText = Data.Text.IO.writeFile
  getTemplateFileAbsPath = Templates.getTemplateFileAbsPath
  getTemplatesDirAbsPath = Templates.getTemplatesDirAbsPath
  compileAndRenderTemplate = Templates.compileAndRenderTemplate
  throwIO = E.throwIO
