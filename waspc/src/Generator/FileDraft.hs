module Generator.FileDraft
       ( FileDraft(..)
       , Writeable(..)
       , createTemplateFileDraft
       , createCopyFileDraft
       , createTextFileDraft
       ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import StrongPath (Path, Abs, Rel, File)
import Generator.Templates (TemplatesDir)
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.Writeable
import qualified Generator.FileDraft.TemplateFileDraft as TmplFD
import qualified Generator.FileDraft.CopyFileDraft as CopyFD
import qualified Generator.FileDraft.TextFileDraft as TextFD


-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
data FileDraft
    = FileDraftTemplateFd TmplFD.TemplateFileDraft
    | FileDraftCopyFd CopyFD.CopyFileDraft
    | FileDraftTextFd TextFD.TextFileDraft
    deriving (Show, Eq)

instance Writeable FileDraft where
    write dstDir (FileDraftTemplateFd draft) = write dstDir draft
    write dstDir (FileDraftCopyFd draft) = write dstDir draft
    write dstDir (FileDraftTextFd draft) = write dstDir draft


createTemplateFileDraft :: Path (Rel ProjectRootDir) File
                        -> Path (Rel TemplatesDir) File
                        -> Maybe Aeson.Value
                        -> FileDraft
createTemplateFileDraft dstPath tmplSrcPath tmplData =
    FileDraftTemplateFd $ TmplFD.TemplateFileDraft { TmplFD._dstPath = dstPath
                                                   , TmplFD._srcPathInTmplDir = tmplSrcPath
                                                   , TmplFD._tmplData = tmplData
                                                   }

createCopyFileDraft :: Path (Rel ProjectRootDir) File -> Path Abs File -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraftCopyFd $ CopyFD.CopyFileDraft { CopyFD._dstPath = dstPath, CopyFD._srcPath = srcPath}

createTextFileDraft :: Path (Rel ProjectRootDir) File -> Text -> FileDraft
createTextFileDraft dstPath content =
    FileDraftTextFd $ TextFD.TextFileDraft { TextFD._dstPath = dstPath, TextFD._content = content}
