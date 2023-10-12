module Wasp.Generator.FileDraft
  ( FileDraft (..),
    Writeable (..),
    createTemplateFileDraft,
    createCopyFileDraft,
    createCopyFileDraftIfExists,
    createTextFileDraft,
    createCopyDirFileDraft,
    createCopyAndModifyTextFileDraft,
  )
where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import StrongPath (Abs, Dir, File, Path', Rel)
import qualified StrongPath as SP
import Wasp.Generator.Common (ProjectRootDir)
import qualified Wasp.Generator.FileDraft.CopyAndModifyTextFileDraft as CMTextFD
import Wasp.Generator.FileDraft.CopyDirFileDraft (CopyDirFileDraftDstDirStrategy)
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
  | FileDraftCopyAndModifyTextFd CMTextFD.CopyAndModifyTextFileDraft

instance Writeable FileDraft where
  write dstDir (FileDraftTemplateFd draft) = write dstDir draft
  write dstDir (FileDraftCopyFd draft) = write dstDir draft
  write dstDir (FileDraftCopyDirFd draft) = write dstDir draft
  write dstDir (FileDraftTextFd draft) = write dstDir draft
  write dstDir (FileDraftCopyAndModifyTextFd draft) = write dstDir draft

  getChecksum (FileDraftTemplateFd draft) = getChecksum draft
  getChecksum (FileDraftCopyFd draft) = getChecksum draft
  getChecksum (FileDraftCopyDirFd draft) = getChecksum draft
  getChecksum (FileDraftTextFd draft) = getChecksum draft
  getChecksum (FileDraftCopyAndModifyTextFd draft) = getChecksum draft

  getDstPath (FileDraftTemplateFd draft) = getDstPath draft
  getDstPath (FileDraftCopyFd draft) = getDstPath draft
  getDstPath (FileDraftCopyDirFd draft) = getDstPath draft
  getDstPath (FileDraftTextFd draft) = getDstPath draft
  getDstPath (FileDraftCopyAndModifyTextFd draft) = getDstPath draft

createTemplateFileDraft ::
  Path' (Rel ProjectRootDir) (File a) ->
  Path' (Rel TemplatesDir) (File b) ->
  Maybe Aeson.Value ->
  FileDraft
createTemplateFileDraft dstPath tmplSrcPath tmplData =
  FileDraftTemplateFd $
    TmplFD.TemplateFileDraft
      { TmplFD._dstPath = SP.castFile dstPath,
        TmplFD._srcPathInTmplDir = SP.castFile tmplSrcPath,
        TmplFD._tmplData = tmplData
      }

createCopyFileDraft :: Path' (Rel ProjectRootDir) (File a) -> Path' Abs (File b) -> FileDraft
createCopyFileDraft dstPath srcPath =
  FileDraftCopyFd $
    CopyFD.CopyFileDraft
      { CopyFD._dstPath = SP.castFile dstPath,
        CopyFD._srcPath = SP.castFile srcPath,
        CopyFD._failIfSrcDoesNotExist = True
      }

createCopyAndModifyTextFileDraft ::
  Path' (Rel ProjectRootDir) (File a) -> Path' Abs (File b) -> (Text -> Text) -> FileDraft
createCopyAndModifyTextFileDraft dstPath srcPath modify =
  FileDraftCopyAndModifyTextFd $
    CMTextFD.CopyAndModifyTextFileDraft
      { CMTextFD._dstPath = SP.castFile dstPath,
        CMTextFD._srcPath = SP.castFile srcPath,
        CMTextFD._modify = modify,
        CMTextFD._failIfSrcDoesNotExist = True
      }

createCopyFileDraftIfExists :: Path' (Rel ProjectRootDir) (File a) -> Path' Abs (File b) -> FileDraft
createCopyFileDraftIfExists dstPath srcPath =
  FileDraftCopyFd $
    CopyFD.CopyFileDraft
      { CopyFD._dstPath = SP.castFile dstPath,
        CopyFD._srcPath = SP.castFile srcPath,
        CopyFD._failIfSrcDoesNotExist = False
      }

createCopyDirFileDraft ::
  CopyDirFileDraftDstDirStrategy ->
  Path' (Rel ProjectRootDir) (Dir a) ->
  Path' Abs (Dir b) ->
  FileDraft
createCopyDirFileDraft dstDirStrategy dstPath srcPath =
  FileDraftCopyDirFd $
    CopyDirFD.CopyDirFileDraft
      { CopyDirFD._dstPath = SP.castDir dstPath,
        CopyDirFD._srcPath = SP.castDir srcPath,
        CopyDirFD._dstDirStrategy = dstDirStrategy
      }

createTextFileDraft :: Path' (Rel ProjectRootDir) (File a) -> Text -> FileDraft
createTextFileDraft dstPath content =
  FileDraftTextFd $ TextFD.TextFileDraft {TextFD._dstPath = SP.castFile dstPath, TextFD._content = content}
