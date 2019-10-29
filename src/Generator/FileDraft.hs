module Generator.FileDraft
       ( FileDraft(..)
       , WriteableToFile(..)
       , createTemplateFileDraft
       , createCopyFileDraft
       , createTextFileDraft
       ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)

import Generator.FileDraft.WriteableToFile
import Generator.FileDraft.TemplateFileDraft
import Generator.FileDraft.CopyFileDraft
import Generator.FileDraft.TextFileDraft


-- | FileDraft unites different file draft types into a single type,
--   so that in the rest of the system they can be passed around as heterogeneous
--   collection when needed.
data FileDraft
    = FileDraftTemplateFd TemplateFileDraft
    | FileDraftCopyFd CopyFileDraft
    | FileDraftTextFd TextFileDraft
    deriving (Show, Eq)

instance WriteableToFile FileDraft where
    writeToFile dstDir (FileDraftTemplateFd templateFd) = writeToFile dstDir templateFd
    writeToFile dstDir (FileDraftCopyFd copyFd) = writeToFile dstDir copyFd
    writeToFile dstDir (FileDraftTextFd textFd) = writeToFile dstDir textFd


createTemplateFileDraft :: FilePath -> FilePath -> Aeson.Value -> FileDraft
createTemplateFileDraft dstPath templateRelPath templateData =
    FileDraftTemplateFd $ TemplateFileDraft dstPath templateRelPath templateData

createCopyFileDraft :: FilePath -> FilePath -> FileDraft
createCopyFileDraft dstPath srcPath =
    FileDraftCopyFd $ CopyFileDraft dstPath srcPath

createTextFileDraft :: FilePath -> Text -> FileDraft
createTextFileDraft dstPath content =
    FileDraftTextFd $ TextFileDraft dstPath content
