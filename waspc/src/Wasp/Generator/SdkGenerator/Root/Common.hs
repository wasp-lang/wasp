module Wasp.Generator.SdkGenerator.Root.Common
  ( RootTemplatesDir,
    mkTmplFd,
    mkTmplFdWithData,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (File', Path', Rel, castRel, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.SdkGenerator.Common
  ( sdkRootDirInProjectRootDir,
    sdkTemplatesDirInTemplatesDir,
  )

data RootTemplatesDir

mkTmplFd ::
  Path' (Rel RootTemplatesDir) File' ->
  FileDraft
mkTmplFd tmplFile = mkTmplFdWithData tmplFile Nothing

mkTmplFdWithData ::
  Path' (Rel RootTemplatesDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  createTemplateFileDraft
    (sdkRootDirInProjectRootDir </> castRel tmplFile)
    (sdkTemplatesDirInTemplatesDir </> castRel tmplFile)
    tmplData
