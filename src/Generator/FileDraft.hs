module Generator.FileDraft
       ( FileDraft(..)
       , Writeable(..)
       , createTemplateFileDraft
       , createCopyFileDraft
       , createTextFileDraft
       , createCopyDirDraft
       ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import Generator.FileDraft.Writeable

import Generator.FileDraft.TemplateFileDraft (TemplateFileDraft)
import qualified Generator.FileDraft.TemplateFileDraft as TemplateFD

import Generator.FileDraft.CopyFileDraft (CopyFileDraft)
import qualified Generator.FileDraft.CopyFileDraft as CopyFD

import Generator.FileDraft.TextFileDraft (TextFileDraft)
import qualified Generator.FileDraft.TextFileDraft as TextFD

import Generator.FileDraft.CopyDirDraft (CopyDirDraft)
import qualified Generator.FileDraft.CopyDirDraft as CopyDirFD


-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
data FileDraft
    = FileDraftTemplateFd TemplateFileDraft
    | FileDraftCopyFd CopyFileDraft
    | FileDraftTextFd TextFileDraft
    | FileDraftCopyDirDraft CopyDirDraft
    deriving (Show, Eq)

instance Writeable FileDraft where
    write dstDir (FileDraftTemplateFd draft) = write dstDir draft
    write dstDir (FileDraftCopyFd draft) = write dstDir draft
    write dstDir (FileDraftTextFd draft) = write dstDir draft
    write dstDir (FileDraftCopyDirDraft draft) = write dstDir draft


createTemplateFileDraft :: FilePath -> FilePath -> Aeson.Value -> FileDraft
createTemplateFileDraft dstPath templateRelPath templateData =
    FileDraftTemplateFd $ TemplateFD.TemplateFileDraft dstPath templateRelPath templateData

createCopyFileDraft :: FilePath -> FilePath -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraftCopyFd $ CopyFD.CopyFileDraft dstPath srcPath

createTextFileDraft :: FilePath -> Text -> FileDraft
createTextFileDraft dstPath content =
    FileDraftTextFd $ TextFD.TextFileDraft dstPath content

createCopyDirDraft :: FilePath -> FilePath -> FileDraft
createCopyDirDraft dstPath srcPath =
    FileDraftCopyDirDraft $ CopyDirFD.CopyDirDraft dstPath srcPath
