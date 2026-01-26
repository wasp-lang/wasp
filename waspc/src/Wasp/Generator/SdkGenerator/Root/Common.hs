module Wasp.Generator.SdkGenerator.Root.Common
  ( TemplatesSdkRootProjectDir,
    SdkRootProjectDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDestAndData,
  )
where

import qualified Data.Aeson as Aeson
import StrongPath (Dir, File', Path', Rel, castRel, reldir, (</>))
import Wasp.Generator.FileDraft (FileDraft, createTemplateFileDraft)
import Wasp.Generator.SdkGenerator.Common
  ( SdkRootDir,
    TemplatesSdkRootDir,
    sdkRootDirInGeneratedCodeDir,
    sdkRootDirInTemplatesDir,
  )

-- | Directory of the SDK root tsconfig project in generated code.
-- References all other SDK tsconfig projects.
data SdkRootProjectDir

-- | Directory of the SDK root tsconfig project in templates.
-- References all other SDK tsconfig projects.
data TemplatesSdkRootProjectDir

mkTmplFd ::
  Path' (Rel TemplatesSdkRootProjectDir) File' ->
  FileDraft
mkTmplFd tmplFile =
  mkTmplFdWithDestAndData
    (castRel tmplFile)
    tmplFile
    Nothing

mkTmplFdWithData ::
  Path' (Rel TemplatesSdkRootProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData tmplFile tmplData =
  mkTmplFdWithDestAndData
    (castRel tmplFile)
    tmplFile
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkRootProjectDir) File' ->
  Path' (Rel TemplatesSdkRootProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFile tmplFile =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkRootProjectDirInSdkRootDir </> destFile)
    (sdkRootDirInTemplatesDir </> templatesSdkRootProjectDirInTemplatesSdkRootDir </> tmplFile)

templatesSdkRootProjectDirInTemplatesSdkRootDir :: Path' (Rel TemplatesSdkRootDir) (Dir TemplatesSdkRootProjectDir)
templatesSdkRootProjectDirInTemplatesSdkRootDir = [reldir|.|]

sdkRootProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkRootProjectDir)
sdkRootProjectDirInSdkRootDir = [reldir|.|]
