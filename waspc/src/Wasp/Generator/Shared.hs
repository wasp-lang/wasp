module Wasp.Generator.Shared
  ( mkTmplFdWithDst,
    SharedTemplatesDir,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, reldir, (</>))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.Templates (TemplatesDir)

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
