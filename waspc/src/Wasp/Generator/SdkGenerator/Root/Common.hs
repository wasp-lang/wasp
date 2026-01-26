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
    SdkTemplatesDir,
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
mkTmplFd srcFilePath =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    Nothing

mkTmplFdWithData ::
  Path' (Rel TemplatesSdkRootProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDestAndData
    (castRel srcFilePath)
    srcFilePath
    (Just tmplData)

mkTmplFdWithDestAndData ::
  Path' (Rel SdkRootProjectDir) File' ->
  Path' (Rel TemplatesSdkRootProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDestAndData destFilePath srcFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkRootProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> templatesSdkRootProjectDirInSdkTemplatesDir </> srcFilePath)

templatesSdkRootProjectDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir TemplatesSdkRootProjectDir)
templatesSdkRootProjectDirInSdkTemplatesDir = [reldir|.|]

sdkRootProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkRootProjectDir)
sdkRootProjectDirInSdkRootDir = [reldir|.|]
