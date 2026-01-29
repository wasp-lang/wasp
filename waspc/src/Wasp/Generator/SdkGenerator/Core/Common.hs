module Wasp.Generator.SdkGenerator.Core.Common
  ( SdkCoreProjectDir,
    SdkTemplatesCoreProjectDir,
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

-- | Directory of the SDK core tsconfig project in generated code.
-- It contains all logic not dependent on the user's project.
data SdkCoreProjectDir

-- | Directory of the SDK core tsconfig project in templates.
-- It contains all logic not dependent on the user's project.
data SdkTemplatesCoreProjectDir

mkTmplFd ::
  Path' (Rel SdkTemplatesCoreProjectDir) File' ->
  FileDraft
mkTmplFd srcFilePath =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    Nothing

mkTmplFdWithData ::
  Path' (Rel SdkTemplatesCoreProjectDir) File' ->
  Aeson.Value ->
  FileDraft
mkTmplFdWithData srcFilePath tmplData =
  mkTmplFdWithDstAndData
    srcFilePath
    (castRel srcFilePath)
    (Just tmplData)

mkTmplFdWithDstAndData ::
  Path' (Rel SdkTemplatesCoreProjectDir) File' ->
  Path' (Rel SdkCoreProjectDir) File' ->
  Maybe Aeson.Value ->
  FileDraft
mkTmplFdWithDstAndData srcFilePath destFilePath =
  createTemplateFileDraft
    (sdkRootDirInGeneratedCodeDir </> sdkCoreProjectDirInSdkRootDir </> destFilePath)
    (sdkRootDirInTemplatesDir </> sdkTemplatesCoreProjectDirInSdkTemplatesDir </> srcFilePath)

sdkTemplatesCoreProjectDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir SdkTemplatesCoreProjectDir)
sdkTemplatesCoreProjectDirInSdkTemplatesDir = [reldir|core|]

sdkCoreProjectDirInSdkRootDir :: Path' (Rel SdkRootDir) (Dir SdkCoreProjectDir)
sdkCoreProjectDirInSdkRootDir = [reldir|core|]
