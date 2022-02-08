module Wasp.Generator.FileDraft
  ( FileDraft (..),
    Writeable (..),
    createTemplateFileDraft,
    createCopyFileDraft,
    createCopyFileDraftIfExists,
    createTextFileDraft,
    createCopyDirFileDraft,
    createBytesFileDraftFromJson,
  )
where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import StrongPath (Abs, Dir', File', Path', Rel)
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.FileDraft.BytesFileDraft as BytesFD
import qualified Wasp.Generator.FileDraft.CopyDirFileDraft as CopyDirFD
import qualified Wasp.Generator.FileDraft.CopyFileDraft as CopyFD
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Wasp.Generator.FileDraft.TextFileDraft as TextFD
import Wasp.Generator.FileDraft.Writeable
import Wasp.Generator.Templates (TemplatesDir)

-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
--
-- TODO: revisit the quick and dirty Linux interpretation of "everything is a file"
-- and treating a directory (`CopyDirFileDraft`) as a `FileDraft`. As is, this may be
-- a source of potential confusion and possibly tech debt to resolve later.
data FileDraft
  = FileDraftTemplateFd TmplFD.TemplateFileDraft
  | FileDraftCopyFd CopyFD.CopyFileDraft
  | FileDraftCopyDirFd CopyDirFD.CopyDirFileDraft
  | FileDraftTextFd TextFD.TextFileDraft
  | FileDraftBytesFd BytesFD.BytesFileDraft
  deriving (Show, Eq)

instance Writeable FileDraft where
  write dstDir (FileDraftTemplateFd draft) = write dstDir draft
  write dstDir (FileDraftCopyFd draft) = write dstDir draft
  write dstDir (FileDraftCopyDirFd draft) = write dstDir draft
  write dstDir (FileDraftTextFd draft) = write dstDir draft
  write dstDir (FileDraftBytesFd draft) = write dstDir draft

createTemplateFileDraft ::
  Path' (Rel ProjectRootDir) File' ->
  Path' (Rel TemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
createTemplateFileDraft dstPath tmplSrcPath tmplData =
  FileDraftTemplateFd $
    TmplFD.TemplateFileDraft
      { TmplFD._dstPath = dstPath,
        TmplFD._srcPathInTmplDir = tmplSrcPath,
        TmplFD._tmplData = tmplData
      }

createCopyFileDraft :: Path' (Rel ProjectRootDir) File' -> Path' Abs File' -> FileDraft
createCopyFileDraft dstPath srcPath =
  FileDraftCopyFd $
    CopyFD.CopyFileDraft
      { CopyFD._dstPath = dstPath,
        CopyFD._srcPath = srcPath,
        CopyFD._failIfSrcDoesNotExist = True
      }

createCopyFileDraftIfExists :: Path' (Rel ProjectRootDir) File' -> Path' Abs File' -> FileDraft
createCopyFileDraftIfExists dstPath srcPath =
  FileDraftCopyFd $
    CopyFD.CopyFileDraft
      { CopyFD._dstPath = dstPath,
        CopyFD._srcPath = srcPath,
        CopyFD._failIfSrcDoesNotExist = False
      }

createCopyDirFileDraft :: Path' (Rel ProjectRootDir) Dir' -> Path' Abs Dir' -> FileDraft
createCopyDirFileDraft dstPath srcPath =
  FileDraftCopyDirFd $
    CopyDirFD.CopyDirFileDraft
      { CopyDirFD._dstPath = dstPath,
        CopyDirFD._srcPath = srcPath
      }

createTextFileDraft :: Path' (Rel ProjectRootDir) File' -> Text -> FileDraft
createTextFileDraft dstPath content =
  FileDraftTextFd $ TextFD.TextFileDraft {TextFD._dstPath = dstPath, TextFD._content = content}

createBytesFileDraftFromJson :: Path' (Rel ProjectRootDir) File' -> Aeson.Value -> FileDraft
createBytesFileDraftFromJson dstPath content =
  FileDraftBytesFd $ BytesFD.BytesFileDraft {BytesFD._dstPath = dstPath, BytesFD._content = Aeson.encode content}
