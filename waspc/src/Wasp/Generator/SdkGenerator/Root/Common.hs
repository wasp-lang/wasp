module Wasp.Generator.SdkGenerator.Root.Common
  ( SdkTemplatesRootProjectDir,
    SdkRootProjectDir,
    mkTmplFd,
    mkTmplFdWithData,
    mkTmplFdWithDstAndData,
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
data SdkTemplatesRootProjectDir

mkTmplFd ::
  Path' (Rel SdkTemplatesRootProjectDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesRootProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesRootProjectDir) File' ->
  Path' (Rel SdkRootProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData srcFilePath destFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkRootProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> sdkTemplatesRootProjectDirInSdkTemplatesDir </> srcFilePath)

sdkTemplatesRootProjectDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir SdkTemplatesRootProjectDir)
sdkTemplatesRootProjectDirInSdkTemplatesDir = [reldir|.|]

sdkRootProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkRootProjectDir)
sdkRootProjectDirInSdkRootDir = [reldir|.|]
