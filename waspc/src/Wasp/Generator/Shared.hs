module Wasp.Generator.Shared (
  mkTmplFdWithDst,
) where

import StrongPath (File', Path', Rel, Dir, reldir, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import qualified Data.Aeson as Aeson
import Wasp.Generator.Templates (TemplatesDir)
import Wasp.Generator.Common (ProjectRootDir)

data SharedTemplatesDir

mkTmplFdWithDst :: Path' (Rel SharedTemplatesDir) File' -> Path' (Rel ProjectRootDir) File' -> FileDraft
mkTmplFdWithDst src dst = mkTmplFdWithDstAndData src dst Nothing

-- | Path in templates directory where shared templates reside.
sharedTemplatesDirInTemplatesDir :: Path' (Rel TemplatesDir) (Dir SharedTemplatesDir)
sharedTemplatesDirInTemplatesDir = [reldir|shared|]

mkTmplFdWithDstAndData ::
  Path' (Rel SharedTemplatesDir) File' ->
  Path' (Rel ProjectRootDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData srcPathInSharedTemplatesDir dstPath tmplData =
  createTemplateFileDraft
    dstPath
    (sharedTemplatesDirInTemplatesDir </> srcPathInSharedTemplatesDir)
    tmplData
