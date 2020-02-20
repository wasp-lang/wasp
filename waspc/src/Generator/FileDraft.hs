module Generator.FileDraft
       ( FileDraft(..)
       , Writeable(..)
       , createTemplateFileDraft
       , createCopyFileDraft
       , createTextFileDraft
       ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Path.Aliases as Path

import Generator.FileDraft.Writeable

import Generator.FileDraft.TemplateFileDraft (TemplateFileDraft)
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD

import Generator.FileDraft.CopyFileDraft (CopyFileDraft)
import qualified Generator.FileDraft.CopyFileDraft as CopyFD

import Generator.FileDraft.TextFileDraft (TextFileDraft)
import qualified Generator.FileDraft.TextFileDraft as TextFD


-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
data FileDraft
    = FileDraftTemplateFd TemplateFileDraft
    | FileDraftCopyFd CopyFileDraft
    | FileDraftTextFd TextFileDraft
    deriving (Show, Eq)

instance Writeable FileDraft where
    write dstDir (FileDraftTemplateFd draft) = write dstDir draft
    write dstDir (FileDraftCopyFd draft) = write dstDir draft
    write dstDir (FileDraftTextFd draft) = write dstDir draft


createTemplateFileDraft :: Path.RelFile -> Path.RelFile -> Maybe Aeson.Value -> FileDraft
createTemplateFileDraft dstPath tmplSrcPath tmplData =
    FileDraftTemplateFd $ TmplFD.TemplateFileDraft { TmplFD._dstPath = dstPath
                                                   , TmplFD._srcPathInTmplDir = tmplSrcPath
                                                   , TmplFD._tmplData = tmplData
                                                   }

createCopyFileDraft :: Path.RelFile -> Path.AbsFile -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraftCopyFd $ CopyFD.CopyFileDraft { CopyFD._dstPath = dstPath, CopyFD._srcPath = srcPath}

createTextFileDraft :: Path.RelFile -> Text -> FileDraft
createTextFileDraft dstPath content =
    FileDraftTextFd $ TextFD.TextFileDraft { TextFD._dstPath = dstPath, TextFD._content = content}
